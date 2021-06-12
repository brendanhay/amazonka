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
-- Module      : Network.AWS.ElasticTranscoder.Types.Warning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Warning where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
--
-- /See:/ 'newWarning' smart constructor.
data Warning = Warning'
  { -- | The message explaining what resources are in a different region from the
    -- pipeline.
    --
    -- AWS KMS keys must be in the same region as the pipeline.
    message :: Core.Maybe Core.Text,
    -- | The code of the cross-regional warning.
    code :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Warning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'warning_message' - The message explaining what resources are in a different region from the
-- pipeline.
--
-- AWS KMS keys must be in the same region as the pipeline.
--
-- 'code', 'warning_code' - The code of the cross-regional warning.
newWarning ::
  Warning
newWarning =
  Warning'
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | The message explaining what resources are in a different region from the
-- pipeline.
--
-- AWS KMS keys must be in the same region as the pipeline.
warning_message :: Lens.Lens' Warning (Core.Maybe Core.Text)
warning_message = Lens.lens (\Warning' {message} -> message) (\s@Warning' {} a -> s {message = a} :: Warning)

-- | The code of the cross-regional warning.
warning_code :: Lens.Lens' Warning (Core.Maybe Core.Text)
warning_code = Lens.lens (\Warning' {code} -> code) (\s@Warning' {} a -> s {code = a} :: Warning)

instance Core.FromJSON Warning where
  parseJSON =
    Core.withObject
      "Warning"
      ( \x ->
          Warning'
            Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Code")
      )

instance Core.Hashable Warning

instance Core.NFData Warning
