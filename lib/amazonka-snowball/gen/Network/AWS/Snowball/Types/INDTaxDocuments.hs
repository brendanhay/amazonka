{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.INDTaxDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.INDTaxDocuments where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tax documents required in AWS Regions in India.
--
--
--
-- /See:/ 'iNDTaxDocuments' smart constructor.
newtype INDTaxDocuments = INDTaxDocuments'
  { _indtdGSTIN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'INDTaxDocuments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'indtdGSTIN' - The Goods and Services Tax (GST) documents required in AWS Regions in India.
iNDTaxDocuments ::
  INDTaxDocuments
iNDTaxDocuments = INDTaxDocuments' {_indtdGSTIN = Nothing}

-- | The Goods and Services Tax (GST) documents required in AWS Regions in India.
indtdGSTIN :: Lens' INDTaxDocuments (Maybe Text)
indtdGSTIN = lens _indtdGSTIN (\s a -> s {_indtdGSTIN = a})

instance FromJSON INDTaxDocuments where
  parseJSON =
    withObject
      "INDTaxDocuments"
      (\x -> INDTaxDocuments' <$> (x .:? "GSTIN"))

instance Hashable INDTaxDocuments

instance NFData INDTaxDocuments

instance ToJSON INDTaxDocuments where
  toJSON INDTaxDocuments' {..} =
    object (catMaybes [("GSTIN" .=) <$> _indtdGSTIN])
