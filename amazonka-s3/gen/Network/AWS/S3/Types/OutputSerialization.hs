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
-- Module      : Network.AWS.S3.Types.OutputSerialization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OutputSerialization where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CSVOutput
import Network.AWS.S3.Types.JSONOutput

-- | Describes how results of the Select job are serialized.
--
-- /See:/ 'newOutputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { -- | Describes the serialization of CSV-encoded Select results.
    csv :: Core.Maybe CSVOutput,
    -- | Specifies JSON as request\'s output serialization format.
    json :: Core.Maybe JSONOutput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputSerialization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'outputSerialization_csv' - Describes the serialization of CSV-encoded Select results.
--
-- 'json', 'outputSerialization_json' - Specifies JSON as request\'s output serialization format.
newOutputSerialization ::
  OutputSerialization
newOutputSerialization =
  OutputSerialization'
    { csv = Core.Nothing,
      json = Core.Nothing
    }

-- | Describes the serialization of CSV-encoded Select results.
outputSerialization_csv :: Lens.Lens' OutputSerialization (Core.Maybe CSVOutput)
outputSerialization_csv = Lens.lens (\OutputSerialization' {csv} -> csv) (\s@OutputSerialization' {} a -> s {csv = a} :: OutputSerialization)

-- | Specifies JSON as request\'s output serialization format.
outputSerialization_json :: Lens.Lens' OutputSerialization (Core.Maybe JSONOutput)
outputSerialization_json = Lens.lens (\OutputSerialization' {json} -> json) (\s@OutputSerialization' {} a -> s {json = a} :: OutputSerialization)

instance Core.Hashable OutputSerialization

instance Core.NFData OutputSerialization

instance Core.ToXML OutputSerialization where
  toXML OutputSerialization' {..} =
    Core.mconcat
      ["CSV" Core.@= csv, "JSON" Core.@= json]
