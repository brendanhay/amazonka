{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateActivityType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /activity type/. After an activity type has
-- been deprecated, you cannot create new tasks of that activity type.
-- Tasks of this type that were scheduled before the type was deprecated
-- will continue to run.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
-- /See:/ <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DeprecateActivityType.html AWS API Reference> for DeprecateActivityType.
module Network.AWS.SWF.DeprecateActivityType
    (
    -- * Creating a Request
      DeprecateActivityType
    , deprecateActivityType
    -- * Request Lenses
    , depDomain
    , depActivityType

    -- * Destructuring the Response
    , DeprecateActivityTypeResponse
    , deprecateActivityTypeResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'deprecateActivityType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depDomain'
--
-- * 'depActivityType'
data DeprecateActivityType = DeprecateActivityType'
    { _depDomain       :: !Text
    , _depActivityType :: !ActivityType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeprecateActivityType' smart constructor.
deprecateActivityType :: Text -> ActivityType -> DeprecateActivityType
deprecateActivityType pDomain_ pActivityType_ =
    DeprecateActivityType'
    { _depDomain = pDomain_
    , _depActivityType = pActivityType_
    }

-- | The name of the domain in which the activity type is registered.
depDomain :: Lens' DeprecateActivityType Text
depDomain = lens _depDomain (\ s a -> s{_depDomain = a});

-- | The activity type to deprecate.
depActivityType :: Lens' DeprecateActivityType ActivityType
depActivityType = lens _depActivityType (\ s a -> s{_depActivityType = a});

instance AWSRequest DeprecateActivityType where
        type Sv DeprecateActivityType = SWF
        type Rs DeprecateActivityType =
             DeprecateActivityTypeResponse
        request = postJSON
        response = receiveNull DeprecateActivityTypeResponse'

instance ToHeaders DeprecateActivityType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DeprecateActivityType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeprecateActivityType where
        toJSON DeprecateActivityType'{..}
          = object
              ["domain" .= _depDomain,
               "activityType" .= _depActivityType]

instance ToPath DeprecateActivityType where
        toPath = const "/"

instance ToQuery DeprecateActivityType where
        toQuery = const mempty

-- | /See:/ 'deprecateActivityTypeResponse' smart constructor.
data DeprecateActivityTypeResponse =
    DeprecateActivityTypeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeprecateActivityTypeResponse' smart constructor.
deprecateActivityTypeResponse :: DeprecateActivityTypeResponse
deprecateActivityTypeResponse = DeprecateActivityTypeResponse'
