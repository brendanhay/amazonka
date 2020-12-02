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
-- Module      : Network.AWS.Lambda.CreateCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a code signing configuration. A <https://docs.aws.amazon.com/lambda/latest/dg/configuration-trustedcode.html code signing configuration> defines a list of allowed signing profiles and defines the code-signing validation policy (action to be taken if deployment validation checks fail).
module Network.AWS.Lambda.CreateCodeSigningConfig
  ( -- * Creating a Request
    createCodeSigningConfig,
    CreateCodeSigningConfig,

    -- * Request Lenses
    ccscCodeSigningPolicies,
    ccscDescription,
    ccscAllowedPublishers,

    -- * Destructuring the Response
    createCodeSigningConfigResponse,
    CreateCodeSigningConfigResponse,

    -- * Response Lenses
    ccscrsResponseStatus,
    ccscrsCodeSigningConfig,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCodeSigningConfig' smart constructor.
data CreateCodeSigningConfig = CreateCodeSigningConfig'
  { _ccscCodeSigningPolicies ::
      !(Maybe CodeSigningPolicies),
    _ccscDescription :: !(Maybe Text),
    _ccscAllowedPublishers ::
      !AllowedPublishers
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCodeSigningConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccscCodeSigningPolicies' - The code signing policies define the actions to take if the validation checks fail.
--
-- * 'ccscDescription' - Descriptive name for this code signing configuration.
--
-- * 'ccscAllowedPublishers' - Signing profiles for this code signing configuration.
createCodeSigningConfig ::
  -- | 'ccscAllowedPublishers'
  AllowedPublishers ->
  CreateCodeSigningConfig
createCodeSigningConfig pAllowedPublishers_ =
  CreateCodeSigningConfig'
    { _ccscCodeSigningPolicies = Nothing,
      _ccscDescription = Nothing,
      _ccscAllowedPublishers = pAllowedPublishers_
    }

-- | The code signing policies define the actions to take if the validation checks fail.
ccscCodeSigningPolicies :: Lens' CreateCodeSigningConfig (Maybe CodeSigningPolicies)
ccscCodeSigningPolicies = lens _ccscCodeSigningPolicies (\s a -> s {_ccscCodeSigningPolicies = a})

-- | Descriptive name for this code signing configuration.
ccscDescription :: Lens' CreateCodeSigningConfig (Maybe Text)
ccscDescription = lens _ccscDescription (\s a -> s {_ccscDescription = a})

-- | Signing profiles for this code signing configuration.
ccscAllowedPublishers :: Lens' CreateCodeSigningConfig AllowedPublishers
ccscAllowedPublishers = lens _ccscAllowedPublishers (\s a -> s {_ccscAllowedPublishers = a})

instance AWSRequest CreateCodeSigningConfig where
  type Rs CreateCodeSigningConfig = CreateCodeSigningConfigResponse
  request = postJSON lambda
  response =
    receiveJSON
      ( \s h x ->
          CreateCodeSigningConfigResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "CodeSigningConfig")
      )

instance Hashable CreateCodeSigningConfig

instance NFData CreateCodeSigningConfig

instance ToHeaders CreateCodeSigningConfig where
  toHeaders = const mempty

instance ToJSON CreateCodeSigningConfig where
  toJSON CreateCodeSigningConfig' {..} =
    object
      ( catMaybes
          [ ("CodeSigningPolicies" .=) <$> _ccscCodeSigningPolicies,
            ("Description" .=) <$> _ccscDescription,
            Just ("AllowedPublishers" .= _ccscAllowedPublishers)
          ]
      )

instance ToPath CreateCodeSigningConfig where
  toPath = const "/2020-04-22/code-signing-configs/"

instance ToQuery CreateCodeSigningConfig where
  toQuery = const mempty

-- | /See:/ 'createCodeSigningConfigResponse' smart constructor.
data CreateCodeSigningConfigResponse = CreateCodeSigningConfigResponse'
  { _ccscrsResponseStatus ::
      !Int,
    _ccscrsCodeSigningConfig ::
      !CodeSigningConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccscrsResponseStatus' - -- | The response status code.
--
-- * 'ccscrsCodeSigningConfig' - The code signing configuration.
createCodeSigningConfigResponse ::
  -- | 'ccscrsResponseStatus'
  Int ->
  -- | 'ccscrsCodeSigningConfig'
  CodeSigningConfig ->
  CreateCodeSigningConfigResponse
createCodeSigningConfigResponse
  pResponseStatus_
  pCodeSigningConfig_ =
    CreateCodeSigningConfigResponse'
      { _ccscrsResponseStatus =
          pResponseStatus_,
        _ccscrsCodeSigningConfig = pCodeSigningConfig_
      }

-- | -- | The response status code.
ccscrsResponseStatus :: Lens' CreateCodeSigningConfigResponse Int
ccscrsResponseStatus = lens _ccscrsResponseStatus (\s a -> s {_ccscrsResponseStatus = a})

-- | The code signing configuration.
ccscrsCodeSigningConfig :: Lens' CreateCodeSigningConfigResponse CodeSigningConfig
ccscrsCodeSigningConfig = lens _ccscrsCodeSigningConfig (\s a -> s {_ccscrsCodeSigningConfig = a})

instance NFData CreateCodeSigningConfigResponse
