{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.EffectivePatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.EffectivePatch where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.Patch
import Network.AWS.SSM.Types.PatchStatus

-- | The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
--
--
--
-- /See:/ 'effectivePatch' smart constructor.
data EffectivePatch = EffectivePatch'
  { _epPatch :: !(Maybe Patch),
    _epPatchStatus :: !(Maybe PatchStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EffectivePatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epPatch' - Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
--
-- * 'epPatchStatus' - The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
effectivePatch ::
  EffectivePatch
effectivePatch =
  EffectivePatch' {_epPatch = Nothing, _epPatchStatus = Nothing}

-- | Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
epPatch :: Lens' EffectivePatch (Maybe Patch)
epPatch = lens _epPatch (\s a -> s {_epPatch = a})

-- | The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
epPatchStatus :: Lens' EffectivePatch (Maybe PatchStatus)
epPatchStatus = lens _epPatchStatus (\s a -> s {_epPatchStatus = a})

instance FromJSON EffectivePatch where
  parseJSON =
    withObject
      "EffectivePatch"
      ( \x ->
          EffectivePatch' <$> (x .:? "Patch") <*> (x .:? "PatchStatus")
      )

instance Hashable EffectivePatch

instance NFData EffectivePatch
