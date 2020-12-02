{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.TaxDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.TaxDocuments where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.INDTaxDocuments

-- | The tax documents required in your AWS Region.
--
--
--
-- /See:/ 'taxDocuments' smart constructor.
newtype TaxDocuments = TaxDocuments'
  { _tdIND ::
      Maybe INDTaxDocuments
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaxDocuments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdIND' - Undocumented member.
taxDocuments ::
  TaxDocuments
taxDocuments = TaxDocuments' {_tdIND = Nothing}

-- | Undocumented member.
tdIND :: Lens' TaxDocuments (Maybe INDTaxDocuments)
tdIND = lens _tdIND (\s a -> s {_tdIND = a})

instance FromJSON TaxDocuments where
  parseJSON =
    withObject
      "TaxDocuments"
      (\x -> TaxDocuments' <$> (x .:? "IND"))

instance Hashable TaxDocuments

instance NFData TaxDocuments

instance ToJSON TaxDocuments where
  toJSON TaxDocuments' {..} =
    object (catMaybes [("IND" .=) <$> _tdIND])
