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
-- Module      : Network.AWS.Redshift.Types.ResizeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes a resize operation.
--
-- /See:/ 'newResizeInfo' smart constructor.
data ResizeInfo = ResizeInfo'
  { -- | A boolean value indicating if the resize operation can be cancelled.
    allowCancelResize :: Prelude.Maybe Prelude.Bool,
    -- | Returns the value @ClassicResize@.
    resizeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResizeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowCancelResize', 'resizeInfo_allowCancelResize' - A boolean value indicating if the resize operation can be cancelled.
--
-- 'resizeType', 'resizeInfo_resizeType' - Returns the value @ClassicResize@.
newResizeInfo ::
  ResizeInfo
newResizeInfo =
  ResizeInfo'
    { allowCancelResize = Prelude.Nothing,
      resizeType = Prelude.Nothing
    }

-- | A boolean value indicating if the resize operation can be cancelled.
resizeInfo_allowCancelResize :: Lens.Lens' ResizeInfo (Prelude.Maybe Prelude.Bool)
resizeInfo_allowCancelResize = Lens.lens (\ResizeInfo' {allowCancelResize} -> allowCancelResize) (\s@ResizeInfo' {} a -> s {allowCancelResize = a} :: ResizeInfo)

-- | Returns the value @ClassicResize@.
resizeInfo_resizeType :: Lens.Lens' ResizeInfo (Prelude.Maybe Prelude.Text)
resizeInfo_resizeType = Lens.lens (\ResizeInfo' {resizeType} -> resizeType) (\s@ResizeInfo' {} a -> s {resizeType = a} :: ResizeInfo)

instance Prelude.FromXML ResizeInfo where
  parseXML x =
    ResizeInfo'
      Prelude.<$> (x Prelude..@? "AllowCancelResize")
      Prelude.<*> (x Prelude..@? "ResizeType")

instance Prelude.Hashable ResizeInfo

instance Prelude.NFData ResizeInfo
