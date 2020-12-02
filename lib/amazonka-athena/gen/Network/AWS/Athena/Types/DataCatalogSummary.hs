{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.DataCatalogSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.DataCatalogSummary where

import Network.AWS.Athena.Types.DataCatalogType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary information for the data catalog, which includes its name and type.
--
--
--
-- /See:/ 'dataCatalogSummary' smart constructor.
data DataCatalogSummary = DataCatalogSummary'
  { _dcsCatalogName ::
      !(Maybe Text),
    _dcsType :: !(Maybe DataCatalogType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataCatalogSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsCatalogName' - The name of the data catalog.
--
-- * 'dcsType' - The data catalog type.
dataCatalogSummary ::
  DataCatalogSummary
dataCatalogSummary =
  DataCatalogSummary'
    { _dcsCatalogName = Nothing,
      _dcsType = Nothing
    }

-- | The name of the data catalog.
dcsCatalogName :: Lens' DataCatalogSummary (Maybe Text)
dcsCatalogName = lens _dcsCatalogName (\s a -> s {_dcsCatalogName = a})

-- | The data catalog type.
dcsType :: Lens' DataCatalogSummary (Maybe DataCatalogType)
dcsType = lens _dcsType (\s a -> s {_dcsType = a})

instance FromJSON DataCatalogSummary where
  parseJSON =
    withObject
      "DataCatalogSummary"
      ( \x ->
          DataCatalogSummary' <$> (x .:? "CatalogName") <*> (x .:? "Type")
      )

instance Hashable DataCatalogSummary

instance NFData DataCatalogSummary
