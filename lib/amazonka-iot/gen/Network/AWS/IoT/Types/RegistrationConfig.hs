{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RegistrationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RegistrationConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The registration configuration.
--
--
--
-- /See:/ 'registrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { _rcTemplateBody ::
      !(Maybe Text),
    _rcRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegistrationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcTemplateBody' - The template body.
--
-- * 'rcRoleARN' - The ARN of the role.
registrationConfig ::
  RegistrationConfig
registrationConfig =
  RegistrationConfig'
    { _rcTemplateBody = Nothing,
      _rcRoleARN = Nothing
    }

-- | The template body.
rcTemplateBody :: Lens' RegistrationConfig (Maybe Text)
rcTemplateBody = lens _rcTemplateBody (\s a -> s {_rcTemplateBody = a})

-- | The ARN of the role.
rcRoleARN :: Lens' RegistrationConfig (Maybe Text)
rcRoleARN = lens _rcRoleARN (\s a -> s {_rcRoleARN = a})

instance FromJSON RegistrationConfig where
  parseJSON =
    withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            <$> (x .:? "templateBody") <*> (x .:? "roleArn")
      )

instance Hashable RegistrationConfig

instance NFData RegistrationConfig

instance ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    object
      ( catMaybes
          [ ("templateBody" .=) <$> _rcTemplateBody,
            ("roleArn" .=) <$> _rcRoleARN
          ]
      )
