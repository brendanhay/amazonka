{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateActivityType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /activity type/ . After an activity type has been deprecated, you cannot create new tasks of that activity type. Tasks of this type that were scheduled before the type was deprecated continue to run.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @activityType.name@ : String constraint. The key is @swf:activityType.name@ .
--
--     * @activityType.version@ : String constraint. The key is @swf:activityType.version@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.DeprecateActivityType
    (
    -- * Creating a Request
      deprecateActivityType
    , DeprecateActivityType
    -- * Request Lenses
    , depDomain
    , depActivityType

    -- * Destructuring the Response
    , deprecateActivityTypeResponse
    , DeprecateActivityTypeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'deprecateActivityType' smart constructor.
data DeprecateActivityType = DeprecateActivityType'
  { _depDomain       :: !Text
  , _depActivityType :: !ActivityType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateActivityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'depDomain' - The name of the domain in which the activity type is registered.
--
-- * 'depActivityType' - The activity type to deprecate.
deprecateActivityType
    :: Text -- ^ 'depDomain'
    -> ActivityType -- ^ 'depActivityType'
    -> DeprecateActivityType
deprecateActivityType pDomain_ pActivityType_ =
  DeprecateActivityType'
    {_depDomain = pDomain_, _depActivityType = pActivityType_}


-- | The name of the domain in which the activity type is registered.
depDomain :: Lens' DeprecateActivityType Text
depDomain = lens _depDomain (\ s a -> s{_depDomain = a})

-- | The activity type to deprecate.
depActivityType :: Lens' DeprecateActivityType ActivityType
depActivityType = lens _depActivityType (\ s a -> s{_depActivityType = a})

instance AWSRequest DeprecateActivityType where
        type Rs DeprecateActivityType =
             DeprecateActivityTypeResponse
        request = postJSON swf
        response = receiveNull DeprecateActivityTypeResponse'

instance Hashable DeprecateActivityType where

instance NFData DeprecateActivityType where

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
              (catMaybes
                 [Just ("domain" .= _depDomain),
                  Just ("activityType" .= _depActivityType)])

instance ToPath DeprecateActivityType where
        toPath = const "/"

instance ToQuery DeprecateActivityType where
        toQuery = const mempty

-- | /See:/ 'deprecateActivityTypeResponse' smart constructor.
data DeprecateActivityTypeResponse =
  DeprecateActivityTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprecateActivityTypeResponse' with the minimum fields required to make a request.
--
deprecateActivityTypeResponse
    :: DeprecateActivityTypeResponse
deprecateActivityTypeResponse = DeprecateActivityTypeResponse'


instance NFData DeprecateActivityTypeResponse where
