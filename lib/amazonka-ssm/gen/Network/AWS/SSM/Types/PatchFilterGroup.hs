{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchFilterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchFilterGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.PatchFilter

-- | A set of patch filters, typically used for approval rules.
--
--
--
-- /See:/ 'patchFilterGroup' smart constructor.
newtype PatchFilterGroup = PatchFilterGroup'
  { _pfgPatchFilters ::
      [PatchFilter]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchFilterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfgPatchFilters' - The set of patch filters that make up the group.
patchFilterGroup ::
  PatchFilterGroup
patchFilterGroup = PatchFilterGroup' {_pfgPatchFilters = mempty}

-- | The set of patch filters that make up the group.
pfgPatchFilters :: Lens' PatchFilterGroup [PatchFilter]
pfgPatchFilters = lens _pfgPatchFilters (\s a -> s {_pfgPatchFilters = a}) . _Coerce

instance FromJSON PatchFilterGroup where
  parseJSON =
    withObject
      "PatchFilterGroup"
      (\x -> PatchFilterGroup' <$> (x .:? "PatchFilters" .!= mempty))

instance Hashable PatchFilterGroup

instance NFData PatchFilterGroup

instance ToJSON PatchFilterGroup where
  toJSON PatchFilterGroup' {..} =
    object (catMaybes [Just ("PatchFilters" .= _pfgPatchFilters)])
