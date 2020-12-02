{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.UpdateCACertificateParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.UpdateCACertificateParams where

import Network.AWS.IoT.Types.CACertificateUpdateAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
--
--
--
-- /See:/ 'updateCACertificateParams' smart constructor.
newtype UpdateCACertificateParams = UpdateCACertificateParams'
  { _ucacpAction ::
      CACertificateUpdateAction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCACertificateParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucacpAction' - The action that you want to apply to the CA cerrtificate. The only supported value is @DEACTIVATE@ .
updateCACertificateParams ::
  -- | 'ucacpAction'
  CACertificateUpdateAction ->
  UpdateCACertificateParams
updateCACertificateParams pAction_ =
  UpdateCACertificateParams' {_ucacpAction = pAction_}

-- | The action that you want to apply to the CA cerrtificate. The only supported value is @DEACTIVATE@ .
ucacpAction :: Lens' UpdateCACertificateParams CACertificateUpdateAction
ucacpAction = lens _ucacpAction (\s a -> s {_ucacpAction = a})

instance FromJSON UpdateCACertificateParams where
  parseJSON =
    withObject
      "UpdateCACertificateParams"
      (\x -> UpdateCACertificateParams' <$> (x .: "action"))

instance Hashable UpdateCACertificateParams

instance NFData UpdateCACertificateParams

instance ToJSON UpdateCACertificateParams where
  toJSON UpdateCACertificateParams' {..} =
    object (catMaybes [Just ("action" .= _ucacpAction)])
