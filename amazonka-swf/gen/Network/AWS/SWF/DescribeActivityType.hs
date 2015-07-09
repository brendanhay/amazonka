{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeActivityType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified activity type. This includes
-- configuration settings provided when the type was registered and other
-- general information about the type.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @activityType.name@: String constraint. The key is
--         @swf:activityType.name@.
--     -   @activityType.version@: String constraint. The key is
--         @swf:activityType.version@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DescribeActivityType.html>
module Network.AWS.SWF.DescribeActivityType
    (
    -- * Request
      DescribeActivityType
    -- ** Request constructor
    , describeActivityType
    -- ** Request lenses
    , datDomain
    , datActivityType

    -- * Response
    , DescribeActivityTypeResponse
    -- ** Response constructor
    , describeActivityTypeResponse
    -- ** Response lenses
    , datrStatus
    , datrTypeInfo
    , datrConfiguration
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'describeActivityType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datDomain'
--
-- * 'datActivityType'
data DescribeActivityType = DescribeActivityType'
    { _datDomain       :: !Text
    , _datActivityType :: !ActivityType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeActivityType' smart constructor.
describeActivityType :: Text -> ActivityType -> DescribeActivityType
describeActivityType pDomain pActivityType =
    DescribeActivityType'
    { _datDomain = pDomain
    , _datActivityType = pActivityType
    }

-- | The name of the domain in which the activity type is registered.
datDomain :: Lens' DescribeActivityType Text
datDomain = lens _datDomain (\ s a -> s{_datDomain = a});

-- | The activity type to get information about. Activity types are
-- identified by the @name@ and @version@ that were supplied when the
-- activity was registered.
datActivityType :: Lens' DescribeActivityType ActivityType
datActivityType = lens _datActivityType (\ s a -> s{_datActivityType = a});

instance AWSRequest DescribeActivityType where
        type Sv DescribeActivityType = SWF
        type Rs DescribeActivityType =
             DescribeActivityTypeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeActivityTypeResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "typeInfo") <*>
                     (x .:> "configuration"))

instance ToHeaders DescribeActivityType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DescribeActivityType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeActivityType where
        toJSON DescribeActivityType'{..}
          = object
              ["domain" .= _datDomain,
               "activityType" .= _datActivityType]

instance ToPath DescribeActivityType where
        toPath = const "/"

instance ToQuery DescribeActivityType where
        toQuery = const mempty

-- | Detailed information about an activity type.
--
-- /See:/ 'describeActivityTypeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datrStatus'
--
-- * 'datrTypeInfo'
--
-- * 'datrConfiguration'
data DescribeActivityTypeResponse = DescribeActivityTypeResponse'
    { _datrStatus        :: !Int
    , _datrTypeInfo      :: !ActivityTypeInfo
    , _datrConfiguration :: !ActivityTypeConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeActivityTypeResponse' smart constructor.
describeActivityTypeResponse :: Int -> ActivityTypeInfo -> ActivityTypeConfiguration -> DescribeActivityTypeResponse
describeActivityTypeResponse pStatus pTypeInfo pConfiguration =
    DescribeActivityTypeResponse'
    { _datrStatus = pStatus
    , _datrTypeInfo = pTypeInfo
    , _datrConfiguration = pConfiguration
    }

-- | FIXME: Undocumented member.
datrStatus :: Lens' DescribeActivityTypeResponse Int
datrStatus = lens _datrStatus (\ s a -> s{_datrStatus = a});

-- | General information about the activity type.
--
-- The status of activity type (returned in the ActivityTypeInfo structure)
-- can be one of the following.
--
-- -   __REGISTERED__: The type is registered and available. Workers
--     supporting this type should be running.
-- -   __DEPRECATED__: The type was deprecated using DeprecateActivityType,
--     but is still in use. You should keep workers supporting this type
--     running. You cannot create new tasks of this type.
datrTypeInfo :: Lens' DescribeActivityTypeResponse ActivityTypeInfo
datrTypeInfo = lens _datrTypeInfo (\ s a -> s{_datrTypeInfo = a});

-- | The configuration settings registered with the activity type.
datrConfiguration :: Lens' DescribeActivityTypeResponse ActivityTypeConfiguration
datrConfiguration = lens _datrConfiguration (\ s a -> s{_datrConfiguration = a});
