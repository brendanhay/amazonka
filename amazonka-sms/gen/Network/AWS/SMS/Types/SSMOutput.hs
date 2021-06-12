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
-- Module      : Network.AWS.SMS.Types.SSMOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.SSMOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.S3Location

-- | Contains the location of validation output.
--
-- /See:/ 'newSSMOutput' smart constructor.
data SSMOutput = SSMOutput'
  { s3Location :: Core.Maybe S3Location
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SSMOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'sSMOutput_s3Location' - Undocumented member.
newSSMOutput ::
  SSMOutput
newSSMOutput = SSMOutput' {s3Location = Core.Nothing}

-- | Undocumented member.
sSMOutput_s3Location :: Lens.Lens' SSMOutput (Core.Maybe S3Location)
sSMOutput_s3Location = Lens.lens (\SSMOutput' {s3Location} -> s3Location) (\s@SSMOutput' {} a -> s {s3Location = a} :: SSMOutput)

instance Core.FromJSON SSMOutput where
  parseJSON =
    Core.withObject
      "SSMOutput"
      ( \x ->
          SSMOutput' Core.<$> (x Core..:? "s3Location")
      )

instance Core.Hashable SSMOutput

instance Core.NFData SSMOutput
