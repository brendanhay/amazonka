{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DesiredWeightAndCapacity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies weight and capacity values for a production variant.
--
--
--
-- /See:/ 'desiredWeightAndCapacity' smart constructor.
data DesiredWeightAndCapacity = DesiredWeightAndCapacity'
  { _dwacDesiredInstanceCount ::
      !(Maybe Nat),
    _dwacDesiredWeight :: !(Maybe Double),
    _dwacVariantName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DesiredWeightAndCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwacDesiredInstanceCount' - The variant's capacity.
--
-- * 'dwacDesiredWeight' - The variant's weight.
--
-- * 'dwacVariantName' - The name of the variant to update.
desiredWeightAndCapacity ::
  -- | 'dwacVariantName'
  Text ->
  DesiredWeightAndCapacity
desiredWeightAndCapacity pVariantName_ =
  DesiredWeightAndCapacity'
    { _dwacDesiredInstanceCount = Nothing,
      _dwacDesiredWeight = Nothing,
      _dwacVariantName = pVariantName_
    }

-- | The variant's capacity.
dwacDesiredInstanceCount :: Lens' DesiredWeightAndCapacity (Maybe Natural)
dwacDesiredInstanceCount = lens _dwacDesiredInstanceCount (\s a -> s {_dwacDesiredInstanceCount = a}) . mapping _Nat

-- | The variant's weight.
dwacDesiredWeight :: Lens' DesiredWeightAndCapacity (Maybe Double)
dwacDesiredWeight = lens _dwacDesiredWeight (\s a -> s {_dwacDesiredWeight = a})

-- | The name of the variant to update.
dwacVariantName :: Lens' DesiredWeightAndCapacity Text
dwacVariantName = lens _dwacVariantName (\s a -> s {_dwacVariantName = a})

instance Hashable DesiredWeightAndCapacity

instance NFData DesiredWeightAndCapacity

instance ToJSON DesiredWeightAndCapacity where
  toJSON DesiredWeightAndCapacity' {..} =
    object
      ( catMaybes
          [ ("DesiredInstanceCount" .=) <$> _dwacDesiredInstanceCount,
            ("DesiredWeight" .=) <$> _dwacDesiredWeight,
            Just ("VariantName" .= _dwacVariantName)
          ]
      )
