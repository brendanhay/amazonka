{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataLakePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataLakePrincipal where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS Lake Formation principal.
--
--
--
-- /See:/ 'dataLakePrincipal' smart constructor.
newtype DataLakePrincipal = DataLakePrincipal'
  { _dlpDataLakePrincipalIdentifier ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataLakePrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlpDataLakePrincipalIdentifier' - An identifier for the AWS Lake Formation principal.
dataLakePrincipal ::
  DataLakePrincipal
dataLakePrincipal =
  DataLakePrincipal' {_dlpDataLakePrincipalIdentifier = Nothing}

-- | An identifier for the AWS Lake Formation principal.
dlpDataLakePrincipalIdentifier :: Lens' DataLakePrincipal (Maybe Text)
dlpDataLakePrincipalIdentifier = lens _dlpDataLakePrincipalIdentifier (\s a -> s {_dlpDataLakePrincipalIdentifier = a})

instance FromJSON DataLakePrincipal where
  parseJSON =
    withObject
      "DataLakePrincipal"
      ( \x ->
          DataLakePrincipal' <$> (x .:? "DataLakePrincipalIdentifier")
      )

instance Hashable DataLakePrincipal

instance NFData DataLakePrincipal

instance ToJSON DataLakePrincipal where
  toJSON DataLakePrincipal' {..} =
    object
      ( catMaybes
          [ ("DataLakePrincipalIdentifier" .=)
              <$> _dlpDataLakePrincipalIdentifier
          ]
      )
