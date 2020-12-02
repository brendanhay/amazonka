{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingPromotion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingPromotion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about an offering promotion.
--
--
--
-- /See:/ 'offeringPromotion' smart constructor.
data OfferingPromotion = OfferingPromotion'
  { _opId :: !(Maybe Text),
    _opDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OfferingPromotion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'opId' - The ID of the offering promotion.
--
-- * 'opDescription' - A string that describes the offering promotion.
offeringPromotion ::
  OfferingPromotion
offeringPromotion =
  OfferingPromotion' {_opId = Nothing, _opDescription = Nothing}

-- | The ID of the offering promotion.
opId :: Lens' OfferingPromotion (Maybe Text)
opId = lens _opId (\s a -> s {_opId = a})

-- | A string that describes the offering promotion.
opDescription :: Lens' OfferingPromotion (Maybe Text)
opDescription = lens _opDescription (\s a -> s {_opDescription = a})

instance FromJSON OfferingPromotion where
  parseJSON =
    withObject
      "OfferingPromotion"
      ( \x ->
          OfferingPromotion' <$> (x .:? "id") <*> (x .:? "description")
      )

instance Hashable OfferingPromotion

instance NFData OfferingPromotion
