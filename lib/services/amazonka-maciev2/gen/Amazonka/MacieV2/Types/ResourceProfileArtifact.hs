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
-- Module      : Amazonka.MacieV2.Types.ResourceProfileArtifact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ResourceProfileArtifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an S3 object that Amazon Macie selected for
-- analysis while performing automated sensitive data discovery for an S3
-- bucket, and the status and results of the analysis. This information is
-- available only if automated sensitive data discovery is currently
-- enabled for your account.
--
-- /See:/ 'newResourceProfileArtifact' smart constructor.
data ResourceProfileArtifact = ResourceProfileArtifact'
  { -- | Specifies whether Amazon Macie found sensitive data in the object.
    sensitive :: Prelude.Maybe Prelude.Bool,
    -- | The status of the analysis. Possible values are:
    --
    -- -   COMPLETE - Amazon Macie successfully completed its analysis of the
    --     object.
    --
    -- -   PARTIAL - Macie analyzed only a subset of data in the object. For
    --     example, the object is an archive file that contains files in an
    --     unsupported format.
    --
    -- -   SKIPPED - Macie wasn\'t able to analyze the object. For example, the
    --     object is a malformed file.
    classificationResultStatus :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the object.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceProfileArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sensitive', 'resourceProfileArtifact_sensitive' - Specifies whether Amazon Macie found sensitive data in the object.
--
-- 'classificationResultStatus', 'resourceProfileArtifact_classificationResultStatus' - The status of the analysis. Possible values are:
--
-- -   COMPLETE - Amazon Macie successfully completed its analysis of the
--     object.
--
-- -   PARTIAL - Macie analyzed only a subset of data in the object. For
--     example, the object is an archive file that contains files in an
--     unsupported format.
--
-- -   SKIPPED - Macie wasn\'t able to analyze the object. For example, the
--     object is a malformed file.
--
-- 'arn', 'resourceProfileArtifact_arn' - The Amazon Resource Name (ARN) of the object.
newResourceProfileArtifact ::
  -- | 'classificationResultStatus'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  ResourceProfileArtifact
newResourceProfileArtifact
  pClassificationResultStatus_
  pArn_ =
    ResourceProfileArtifact'
      { sensitive =
          Prelude.Nothing,
        classificationResultStatus =
          pClassificationResultStatus_,
        arn = pArn_
      }

-- | Specifies whether Amazon Macie found sensitive data in the object.
resourceProfileArtifact_sensitive :: Lens.Lens' ResourceProfileArtifact (Prelude.Maybe Prelude.Bool)
resourceProfileArtifact_sensitive = Lens.lens (\ResourceProfileArtifact' {sensitive} -> sensitive) (\s@ResourceProfileArtifact' {} a -> s {sensitive = a} :: ResourceProfileArtifact)

-- | The status of the analysis. Possible values are:
--
-- -   COMPLETE - Amazon Macie successfully completed its analysis of the
--     object.
--
-- -   PARTIAL - Macie analyzed only a subset of data in the object. For
--     example, the object is an archive file that contains files in an
--     unsupported format.
--
-- -   SKIPPED - Macie wasn\'t able to analyze the object. For example, the
--     object is a malformed file.
resourceProfileArtifact_classificationResultStatus :: Lens.Lens' ResourceProfileArtifact Prelude.Text
resourceProfileArtifact_classificationResultStatus = Lens.lens (\ResourceProfileArtifact' {classificationResultStatus} -> classificationResultStatus) (\s@ResourceProfileArtifact' {} a -> s {classificationResultStatus = a} :: ResourceProfileArtifact)

-- | The Amazon Resource Name (ARN) of the object.
resourceProfileArtifact_arn :: Lens.Lens' ResourceProfileArtifact Prelude.Text
resourceProfileArtifact_arn = Lens.lens (\ResourceProfileArtifact' {arn} -> arn) (\s@ResourceProfileArtifact' {} a -> s {arn = a} :: ResourceProfileArtifact)

instance Data.FromJSON ResourceProfileArtifact where
  parseJSON =
    Data.withObject
      "ResourceProfileArtifact"
      ( \x ->
          ResourceProfileArtifact'
            Prelude.<$> (x Data..:? "sensitive")
            Prelude.<*> (x Data..: "classificationResultStatus")
            Prelude.<*> (x Data..: "arn")
      )

instance Prelude.Hashable ResourceProfileArtifact where
  hashWithSalt _salt ResourceProfileArtifact' {..} =
    _salt `Prelude.hashWithSalt` sensitive
      `Prelude.hashWithSalt` classificationResultStatus
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ResourceProfileArtifact where
  rnf ResourceProfileArtifact' {..} =
    Prelude.rnf sensitive
      `Prelude.seq` Prelude.rnf classificationResultStatus
      `Prelude.seq` Prelude.rnf arn
