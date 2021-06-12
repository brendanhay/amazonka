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
-- Module      : Network.AWS.EC2.Types.AnalysisComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AnalysisComponent where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a path component.
--
-- /See:/ 'newAnalysisComponent' smart constructor.
data AnalysisComponent = AnalysisComponent'
  { -- | The Amazon Resource Name (ARN) of the component.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the component.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnalysisComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'analysisComponent_arn' - The Amazon Resource Name (ARN) of the component.
--
-- 'id', 'analysisComponent_id' - The ID of the component.
newAnalysisComponent ::
  AnalysisComponent
newAnalysisComponent =
  AnalysisComponent'
    { arn = Core.Nothing,
      id = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the component.
analysisComponent_arn :: Lens.Lens' AnalysisComponent (Core.Maybe Core.Text)
analysisComponent_arn = Lens.lens (\AnalysisComponent' {arn} -> arn) (\s@AnalysisComponent' {} a -> s {arn = a} :: AnalysisComponent)

-- | The ID of the component.
analysisComponent_id :: Lens.Lens' AnalysisComponent (Core.Maybe Core.Text)
analysisComponent_id = Lens.lens (\AnalysisComponent' {id} -> id) (\s@AnalysisComponent' {} a -> s {id = a} :: AnalysisComponent)

instance Core.FromXML AnalysisComponent where
  parseXML x =
    AnalysisComponent'
      Core.<$> (x Core..@? "arn") Core.<*> (x Core..@? "id")

instance Core.Hashable AnalysisComponent

instance Core.NFData AnalysisComponent
