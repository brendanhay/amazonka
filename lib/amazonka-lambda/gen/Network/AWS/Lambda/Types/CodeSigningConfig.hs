{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.CodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningConfig where

import Network.AWS.Lambda.Types.AllowedPublishers
import Network.AWS.Lambda.Types.CodeSigningPolicies
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about a Code signing configuration.
--
--
--
-- /See:/ 'codeSigningConfig' smart constructor.
data CodeSigningConfig = CodeSigningConfig'
  { _cscDescription ::
      !(Maybe Text),
    _cscCodeSigningConfigId :: !Text,
    _cscCodeSigningConfigARN :: !Text,
    _cscAllowedPublishers :: !AllowedPublishers,
    _cscCodeSigningPolicies :: !CodeSigningPolicies,
    _cscLastModified :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeSigningConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscDescription' - Code signing configuration description.
--
-- * 'cscCodeSigningConfigId' - Unique identifer for the Code signing configuration.
--
-- * 'cscCodeSigningConfigARN' - The Amazon Resource Name (ARN) of the Code signing configuration.
--
-- * 'cscAllowedPublishers' - List of allowed publishers.
--
-- * 'cscCodeSigningPolicies' - The code signing policy controls the validation failure action for signature mismatch or expiry.
--
-- * 'cscLastModified' - The date and time that the Code signing configuration was last modified, in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
codeSigningConfig ::
  -- | 'cscCodeSigningConfigId'
  Text ->
  -- | 'cscCodeSigningConfigARN'
  Text ->
  -- | 'cscAllowedPublishers'
  AllowedPublishers ->
  -- | 'cscCodeSigningPolicies'
  CodeSigningPolicies ->
  -- | 'cscLastModified'
  Text ->
  CodeSigningConfig
codeSigningConfig
  pCodeSigningConfigId_
  pCodeSigningConfigARN_
  pAllowedPublishers_
  pCodeSigningPolicies_
  pLastModified_ =
    CodeSigningConfig'
      { _cscDescription = Nothing,
        _cscCodeSigningConfigId = pCodeSigningConfigId_,
        _cscCodeSigningConfigARN = pCodeSigningConfigARN_,
        _cscAllowedPublishers = pAllowedPublishers_,
        _cscCodeSigningPolicies = pCodeSigningPolicies_,
        _cscLastModified = pLastModified_
      }

-- | Code signing configuration description.
cscDescription :: Lens' CodeSigningConfig (Maybe Text)
cscDescription = lens _cscDescription (\s a -> s {_cscDescription = a})

-- | Unique identifer for the Code signing configuration.
cscCodeSigningConfigId :: Lens' CodeSigningConfig Text
cscCodeSigningConfigId = lens _cscCodeSigningConfigId (\s a -> s {_cscCodeSigningConfigId = a})

-- | The Amazon Resource Name (ARN) of the Code signing configuration.
cscCodeSigningConfigARN :: Lens' CodeSigningConfig Text
cscCodeSigningConfigARN = lens _cscCodeSigningConfigARN (\s a -> s {_cscCodeSigningConfigARN = a})

-- | List of allowed publishers.
cscAllowedPublishers :: Lens' CodeSigningConfig AllowedPublishers
cscAllowedPublishers = lens _cscAllowedPublishers (\s a -> s {_cscAllowedPublishers = a})

-- | The code signing policy controls the validation failure action for signature mismatch or expiry.
cscCodeSigningPolicies :: Lens' CodeSigningConfig CodeSigningPolicies
cscCodeSigningPolicies = lens _cscCodeSigningPolicies (\s a -> s {_cscCodeSigningPolicies = a})

-- | The date and time that the Code signing configuration was last modified, in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
cscLastModified :: Lens' CodeSigningConfig Text
cscLastModified = lens _cscLastModified (\s a -> s {_cscLastModified = a})

instance FromJSON CodeSigningConfig where
  parseJSON =
    withObject
      "CodeSigningConfig"
      ( \x ->
          CodeSigningConfig'
            <$> (x .:? "Description")
            <*> (x .: "CodeSigningConfigId")
            <*> (x .: "CodeSigningConfigArn")
            <*> (x .: "AllowedPublishers")
            <*> (x .: "CodeSigningPolicies")
            <*> (x .: "LastModified")
      )

instance Hashable CodeSigningConfig

instance NFData CodeSigningConfig
