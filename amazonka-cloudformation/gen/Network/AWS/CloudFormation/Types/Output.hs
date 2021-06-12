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
-- Module      : Network.AWS.CloudFormation.Types.Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Output where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Output data type.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | The key associated with the output.
    outputKey :: Core.Maybe Core.Text,
    -- | The value associated with the output.
    outputValue :: Core.Maybe Core.Text,
    -- | User defined description associated with the output.
    description :: Core.Maybe Core.Text,
    -- | The name of the export associated with the output.
    exportName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputKey', 'output_outputKey' - The key associated with the output.
--
-- 'outputValue', 'output_outputValue' - The value associated with the output.
--
-- 'description', 'output_description' - User defined description associated with the output.
--
-- 'exportName', 'output_exportName' - The name of the export associated with the output.
newOutput ::
  Output
newOutput =
  Output'
    { outputKey = Core.Nothing,
      outputValue = Core.Nothing,
      description = Core.Nothing,
      exportName = Core.Nothing
    }

-- | The key associated with the output.
output_outputKey :: Lens.Lens' Output (Core.Maybe Core.Text)
output_outputKey = Lens.lens (\Output' {outputKey} -> outputKey) (\s@Output' {} a -> s {outputKey = a} :: Output)

-- | The value associated with the output.
output_outputValue :: Lens.Lens' Output (Core.Maybe Core.Text)
output_outputValue = Lens.lens (\Output' {outputValue} -> outputValue) (\s@Output' {} a -> s {outputValue = a} :: Output)

-- | User defined description associated with the output.
output_description :: Lens.Lens' Output (Core.Maybe Core.Text)
output_description = Lens.lens (\Output' {description} -> description) (\s@Output' {} a -> s {description = a} :: Output)

-- | The name of the export associated with the output.
output_exportName :: Lens.Lens' Output (Core.Maybe Core.Text)
output_exportName = Lens.lens (\Output' {exportName} -> exportName) (\s@Output' {} a -> s {exportName = a} :: Output)

instance Core.FromXML Output where
  parseXML x =
    Output'
      Core.<$> (x Core..@? "OutputKey")
      Core.<*> (x Core..@? "OutputValue")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ExportName")

instance Core.Hashable Output

instance Core.NFData Output
