{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Difference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Difference where

import Network.AWS.CodeCommit.Types.BlobMetadata
import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a set of differences for a commit specifier.
--
-- /See:/ 'newDifference' smart constructor.
data Difference = Difference'
  { -- | Whether the change type of the difference is an addition (A), deletion
    -- (D), or modification (M).
    changeType :: Prelude.Maybe ChangeTypeEnum,
    -- | Information about an @afterBlob@ data type object, including the ID, the
    -- file mode permission code, and the path.
    afterBlob :: Prelude.Maybe BlobMetadata,
    -- | Information about a @beforeBlob@ data type object, including the ID, the
    -- file mode permission code, and the path.
    beforeBlob :: Prelude.Maybe BlobMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Difference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeType', 'difference_changeType' - Whether the change type of the difference is an addition (A), deletion
-- (D), or modification (M).
--
-- 'afterBlob', 'difference_afterBlob' - Information about an @afterBlob@ data type object, including the ID, the
-- file mode permission code, and the path.
--
-- 'beforeBlob', 'difference_beforeBlob' - Information about a @beforeBlob@ data type object, including the ID, the
-- file mode permission code, and the path.
newDifference ::
  Difference
newDifference =
  Difference'
    { changeType = Prelude.Nothing,
      afterBlob = Prelude.Nothing,
      beforeBlob = Prelude.Nothing
    }

-- | Whether the change type of the difference is an addition (A), deletion
-- (D), or modification (M).
difference_changeType :: Lens.Lens' Difference (Prelude.Maybe ChangeTypeEnum)
difference_changeType = Lens.lens (\Difference' {changeType} -> changeType) (\s@Difference' {} a -> s {changeType = a} :: Difference)

-- | Information about an @afterBlob@ data type object, including the ID, the
-- file mode permission code, and the path.
difference_afterBlob :: Lens.Lens' Difference (Prelude.Maybe BlobMetadata)
difference_afterBlob = Lens.lens (\Difference' {afterBlob} -> afterBlob) (\s@Difference' {} a -> s {afterBlob = a} :: Difference)

-- | Information about a @beforeBlob@ data type object, including the ID, the
-- file mode permission code, and the path.
difference_beforeBlob :: Lens.Lens' Difference (Prelude.Maybe BlobMetadata)
difference_beforeBlob = Lens.lens (\Difference' {beforeBlob} -> beforeBlob) (\s@Difference' {} a -> s {beforeBlob = a} :: Difference)

instance Prelude.FromJSON Difference where
  parseJSON =
    Prelude.withObject
      "Difference"
      ( \x ->
          Difference'
            Prelude.<$> (x Prelude..:? "changeType")
            Prelude.<*> (x Prelude..:? "afterBlob")
            Prelude.<*> (x Prelude..:? "beforeBlob")
      )

instance Prelude.Hashable Difference

instance Prelude.NFData Difference
