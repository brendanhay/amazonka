{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /activity type/ . After an activity type has been undeprecated, you can create new tasks of that activity type.
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
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.UndeprecateActivityType
  ( -- * Creating a Request
    undeprecateActivityType,
    UndeprecateActivityType,

    -- * Request Lenses
    uatDomain,
    uatActivityType,

    -- * Destructuring the Response
    undeprecateActivityTypeResponse,
    UndeprecateActivityTypeResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types

-- | /See:/ 'undeprecateActivityType' smart constructor.
data UndeprecateActivityType = UndeprecateActivityType'
  { _uatDomain ::
      !Text,
    _uatActivityType :: !ActivityType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UndeprecateActivityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uatDomain' - The name of the domain of the deprecated activity type.
--
-- * 'uatActivityType' - The activity type to undeprecate.
undeprecateActivityType ::
  -- | 'uatDomain'
  Text ->
  -- | 'uatActivityType'
  ActivityType ->
  UndeprecateActivityType
undeprecateActivityType pDomain_ pActivityType_ =
  UndeprecateActivityType'
    { _uatDomain = pDomain_,
      _uatActivityType = pActivityType_
    }

-- | The name of the domain of the deprecated activity type.
uatDomain :: Lens' UndeprecateActivityType Text
uatDomain = lens _uatDomain (\s a -> s {_uatDomain = a})

-- | The activity type to undeprecate.
uatActivityType :: Lens' UndeprecateActivityType ActivityType
uatActivityType = lens _uatActivityType (\s a -> s {_uatActivityType = a})

instance AWSRequest UndeprecateActivityType where
  type Rs UndeprecateActivityType = UndeprecateActivityTypeResponse
  request = postJSON swf
  response = receiveNull UndeprecateActivityTypeResponse'

instance Hashable UndeprecateActivityType

instance NFData UndeprecateActivityType

instance ToHeaders UndeprecateActivityType where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SimpleWorkflowService.UndeprecateActivityType" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON UndeprecateActivityType where
  toJSON UndeprecateActivityType' {..} =
    object
      ( catMaybes
          [ Just ("domain" .= _uatDomain),
            Just ("activityType" .= _uatActivityType)
          ]
      )

instance ToPath UndeprecateActivityType where
  toPath = const "/"

instance ToQuery UndeprecateActivityType where
  toQuery = const mempty

-- | /See:/ 'undeprecateActivityTypeResponse' smart constructor.
data UndeprecateActivityTypeResponse = UndeprecateActivityTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UndeprecateActivityTypeResponse' with the minimum fields required to make a request.
undeprecateActivityTypeResponse ::
  UndeprecateActivityTypeResponse
undeprecateActivityTypeResponse = UndeprecateActivityTypeResponse'

instance NFData UndeprecateActivityTypeResponse
