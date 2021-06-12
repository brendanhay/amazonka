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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes a resize operation.
--
-- /See:/ 'newResizeInfo' smart constructor.
data ResizeInfo = ResizeInfo'
  { -- | A boolean value indicating if the resize operation can be cancelled.
    allowCancelResize :: Core.Maybe Core.Bool,
    -- | Returns the value @ClassicResize@.
    resizeType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { allowCancelResize = Core.Nothing,
      resizeType = Core.Nothing
    }

-- | A boolean value indicating if the resize operation can be cancelled.
resizeInfo_allowCancelResize :: Lens.Lens' ResizeInfo (Core.Maybe Core.Bool)
resizeInfo_allowCancelResize = Lens.lens (\ResizeInfo' {allowCancelResize} -> allowCancelResize) (\s@ResizeInfo' {} a -> s {allowCancelResize = a} :: ResizeInfo)

-- | Returns the value @ClassicResize@.
resizeInfo_resizeType :: Lens.Lens' ResizeInfo (Core.Maybe Core.Text)
resizeInfo_resizeType = Lens.lens (\ResizeInfo' {resizeType} -> resizeType) (\s@ResizeInfo' {} a -> s {resizeType = a} :: ResizeInfo)

instance Core.FromXML ResizeInfo where
  parseXML x =
    ResizeInfo'
      Core.<$> (x Core..@? "AllowCancelResize")
      Core.<*> (x Core..@? "ResizeType")

instance Core.Hashable ResizeInfo

instance Core.NFData ResizeInfo
