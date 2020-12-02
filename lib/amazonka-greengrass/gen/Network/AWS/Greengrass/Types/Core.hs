{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Core
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Core where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a core.
--
-- /See:/ 'core' smart constructor.
data Core = Core'
  { _cSyncShadow :: !(Maybe Bool),
    _cThingARN :: !Text,
    _cId :: !Text,
    _cCertificateARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Core' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cSyncShadow' - If true, the core's local shadow is automatically synced with the cloud.
--
-- * 'cThingARN' - The ARN of the thing which is the core.
--
-- * 'cId' - A descriptive or arbitrary ID for the core. This value must be unique within the core definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- * 'cCertificateARN' - The ARN of the certificate associated with the core.
core ::
  -- | 'cThingARN'
  Text ->
  -- | 'cId'
  Text ->
  -- | 'cCertificateARN'
  Text ->
  Core
core pThingARN_ pId_ pCertificateARN_ =
  Core'
    { _cSyncShadow = Nothing,
      _cThingARN = pThingARN_,
      _cId = pId_,
      _cCertificateARN = pCertificateARN_
    }

-- | If true, the core's local shadow is automatically synced with the cloud.
cSyncShadow :: Lens' Core (Maybe Bool)
cSyncShadow = lens _cSyncShadow (\s a -> s {_cSyncShadow = a})

-- | The ARN of the thing which is the core.
cThingARN :: Lens' Core Text
cThingARN = lens _cThingARN (\s a -> s {_cThingARN = a})

-- | A descriptive or arbitrary ID for the core. This value must be unique within the core definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
cId :: Lens' Core Text
cId = lens _cId (\s a -> s {_cId = a})

-- | The ARN of the certificate associated with the core.
cCertificateARN :: Lens' Core Text
cCertificateARN = lens _cCertificateARN (\s a -> s {_cCertificateARN = a})

instance FromJSON Core where
  parseJSON =
    withObject
      "Core"
      ( \x ->
          Core'
            <$> (x .:? "SyncShadow")
            <*> (x .: "ThingArn")
            <*> (x .: "Id")
            <*> (x .: "CertificateArn")
      )

instance Hashable Core

instance NFData Core

instance ToJSON Core where
  toJSON Core' {..} =
    object
      ( catMaybes
          [ ("SyncShadow" .=) <$> _cSyncShadow,
            Just ("ThingArn" .= _cThingARN),
            Just ("Id" .= _cId),
            Just ("CertificateArn" .= _cCertificateARN)
          ]
      )
