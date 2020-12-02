{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.PatchFilterKey

-- | Defines which patches should be included in a patch baseline.
--
--
-- A patch filter consists of a key and a set of values. The filter key is a patch property. For example, the available filter keys for WINDOWS are PATCH_SET, PRODUCT, PRODUCT_FAMILY, CLASSIFICATION, and MSRC_SEVERITY. The filter values define a matching criterion for the patch property indicated by the key. For example, if the filter key is PRODUCT and the filter values are ["Office 2013", "Office 2016"], then the filter accepts all patches where product name is either "Office 2013" or "Office 2016". The filter values can be exact values for the patch property given as a key, or a wildcard (*), which matches all values.
--
-- You can view lists of valid values for the patch properties by running the @DescribePatchProperties@ command. For information about which patch properties can be used with each major operating system, see 'DescribePatchProperties' .
--
--
-- /See:/ 'patchFilter' smart constructor.
data PatchFilter = PatchFilter'
  { _pfKey :: !PatchFilterKey,
    _pfValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfKey' - The key for the filter. Run the 'DescribePatchProperties' command to view lists of valid keys for each operating system type.
--
-- * 'pfValues' - The value for the filter key. Run the 'DescribePatchProperties' command to view lists of valid values for each key based on operating system type.
patchFilter ::
  -- | 'pfKey'
  PatchFilterKey ->
  -- | 'pfValues'
  NonEmpty Text ->
  PatchFilter
patchFilter pKey_ pValues_ =
  PatchFilter' {_pfKey = pKey_, _pfValues = _List1 # pValues_}

-- | The key for the filter. Run the 'DescribePatchProperties' command to view lists of valid keys for each operating system type.
pfKey :: Lens' PatchFilter PatchFilterKey
pfKey = lens _pfKey (\s a -> s {_pfKey = a})

-- | The value for the filter key. Run the 'DescribePatchProperties' command to view lists of valid values for each key based on operating system type.
pfValues :: Lens' PatchFilter (NonEmpty Text)
pfValues = lens _pfValues (\s a -> s {_pfValues = a}) . _List1

instance FromJSON PatchFilter where
  parseJSON =
    withObject
      "PatchFilter"
      (\x -> PatchFilter' <$> (x .: "Key") <*> (x .: "Values"))

instance Hashable PatchFilter

instance NFData PatchFilter

instance ToJSON PatchFilter where
  toJSON PatchFilter' {..} =
    object
      (catMaybes [Just ("Key" .= _pfKey), Just ("Values" .= _pfValues)])
