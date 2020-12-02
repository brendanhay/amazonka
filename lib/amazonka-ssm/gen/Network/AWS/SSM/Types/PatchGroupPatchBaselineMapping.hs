{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.PatchBaselineIdentity

-- | The mapping between a patch group and the patch baseline the patch group is registered with.
--
--
--
-- /See:/ 'patchGroupPatchBaselineMapping' smart constructor.
data PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping'
  { _pgpbmBaselineIdentity ::
      !( Maybe
           PatchBaselineIdentity
       ),
    _pgpbmPatchGroup ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchGroupPatchBaselineMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgpbmBaselineIdentity' - The patch baseline the patch group is registered with.
--
-- * 'pgpbmPatchGroup' - The name of the patch group registered with the patch baseline.
patchGroupPatchBaselineMapping ::
  PatchGroupPatchBaselineMapping
patchGroupPatchBaselineMapping =
  PatchGroupPatchBaselineMapping'
    { _pgpbmBaselineIdentity = Nothing,
      _pgpbmPatchGroup = Nothing
    }

-- | The patch baseline the patch group is registered with.
pgpbmBaselineIdentity :: Lens' PatchGroupPatchBaselineMapping (Maybe PatchBaselineIdentity)
pgpbmBaselineIdentity = lens _pgpbmBaselineIdentity (\s a -> s {_pgpbmBaselineIdentity = a})

-- | The name of the patch group registered with the patch baseline.
pgpbmPatchGroup :: Lens' PatchGroupPatchBaselineMapping (Maybe Text)
pgpbmPatchGroup = lens _pgpbmPatchGroup (\s a -> s {_pgpbmPatchGroup = a})

instance FromJSON PatchGroupPatchBaselineMapping where
  parseJSON =
    withObject
      "PatchGroupPatchBaselineMapping"
      ( \x ->
          PatchGroupPatchBaselineMapping'
            <$> (x .:? "BaselineIdentity") <*> (x .:? "PatchGroup")
      )

instance Hashable PatchGroupPatchBaselineMapping

instance NFData PatchGroupPatchBaselineMapping
