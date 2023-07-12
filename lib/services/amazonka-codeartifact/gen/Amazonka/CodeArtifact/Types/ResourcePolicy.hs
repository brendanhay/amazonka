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
-- Module      : Amazonka.CodeArtifact.Types.ResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.ResourcePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An CodeArtifact resource policy that contains a resource ARN, document
-- details, and a revision.
--
-- /See:/ 'newResourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { -- | The resource policy formatted in JSON.
    document :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource associated with the resource policy
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The current revision of the resource policy.
    revision :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'resourcePolicy_document' - The resource policy formatted in JSON.
--
-- 'resourceArn', 'resourcePolicy_resourceArn' - The ARN of the resource associated with the resource policy
--
-- 'revision', 'resourcePolicy_revision' - The current revision of the resource policy.
newResourcePolicy ::
  ResourcePolicy
newResourcePolicy =
  ResourcePolicy'
    { document = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | The resource policy formatted in JSON.
resourcePolicy_document :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_document = Lens.lens (\ResourcePolicy' {document} -> document) (\s@ResourcePolicy' {} a -> s {document = a} :: ResourcePolicy)

-- | The ARN of the resource associated with the resource policy
resourcePolicy_resourceArn :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_resourceArn = Lens.lens (\ResourcePolicy' {resourceArn} -> resourceArn) (\s@ResourcePolicy' {} a -> s {resourceArn = a} :: ResourcePolicy)

-- | The current revision of the resource policy.
resourcePolicy_revision :: Lens.Lens' ResourcePolicy (Prelude.Maybe Prelude.Text)
resourcePolicy_revision = Lens.lens (\ResourcePolicy' {revision} -> revision) (\s@ResourcePolicy' {} a -> s {revision = a} :: ResourcePolicy)

instance Data.FromJSON ResourcePolicy where
  parseJSON =
    Data.withObject
      "ResourcePolicy"
      ( \x ->
          ResourcePolicy'
            Prelude.<$> (x Data..:? "document")
            Prelude.<*> (x Data..:? "resourceArn")
            Prelude.<*> (x Data..:? "revision")
      )

instance Prelude.Hashable ResourcePolicy where
  hashWithSalt _salt ResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` revision

instance Prelude.NFData ResourcePolicy where
  rnf ResourcePolicy' {..} =
    Prelude.rnf document
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf revision
