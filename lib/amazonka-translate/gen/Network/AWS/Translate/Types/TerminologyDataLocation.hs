{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyDataLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyDataLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The location of the custom terminology data.
--
--
--
-- /See:/ 'terminologyDataLocation' smart constructor.
data TerminologyDataLocation = TerminologyDataLocation'
  { _tdlRepositoryType ::
      !Text,
    _tdlLocation :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TerminologyDataLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdlRepositoryType' - The repository type for the custom terminology data.
--
-- * 'tdlLocation' - The location of the custom terminology data.
terminologyDataLocation ::
  -- | 'tdlRepositoryType'
  Text ->
  -- | 'tdlLocation'
  Text ->
  TerminologyDataLocation
terminologyDataLocation pRepositoryType_ pLocation_ =
  TerminologyDataLocation'
    { _tdlRepositoryType = pRepositoryType_,
      _tdlLocation = pLocation_
    }

-- | The repository type for the custom terminology data.
tdlRepositoryType :: Lens' TerminologyDataLocation Text
tdlRepositoryType = lens _tdlRepositoryType (\s a -> s {_tdlRepositoryType = a})

-- | The location of the custom terminology data.
tdlLocation :: Lens' TerminologyDataLocation Text
tdlLocation = lens _tdlLocation (\s a -> s {_tdlLocation = a})

instance FromJSON TerminologyDataLocation where
  parseJSON =
    withObject
      "TerminologyDataLocation"
      ( \x ->
          TerminologyDataLocation'
            <$> (x .: "RepositoryType") <*> (x .: "Location")
      )

instance Hashable TerminologyDataLocation

instance NFData TerminologyDataLocation
