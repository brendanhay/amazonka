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
-- Module      : Amazonka.SageMaker.Types.IamIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.IamIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The IAM Identity details associated with the user. These details are
-- associated with model package groups, model packages and project
-- entities only.
--
-- /See:/ 'newIamIdentity' smart constructor.
data IamIdentity = IamIdentity'
  { -- | The Amazon Resource Name (ARN) of the IAM identity.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the principal that assumes the IAM identity.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The person or application which assumes the IAM identity.
    sourceIdentity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iamIdentity_arn' - The Amazon Resource Name (ARN) of the IAM identity.
--
-- 'principalId', 'iamIdentity_principalId' - The ID of the principal that assumes the IAM identity.
--
-- 'sourceIdentity', 'iamIdentity_sourceIdentity' - The person or application which assumes the IAM identity.
newIamIdentity ::
  IamIdentity
newIamIdentity =
  IamIdentity'
    { arn = Prelude.Nothing,
      principalId = Prelude.Nothing,
      sourceIdentity = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM identity.
iamIdentity_arn :: Lens.Lens' IamIdentity (Prelude.Maybe Prelude.Text)
iamIdentity_arn = Lens.lens (\IamIdentity' {arn} -> arn) (\s@IamIdentity' {} a -> s {arn = a} :: IamIdentity)

-- | The ID of the principal that assumes the IAM identity.
iamIdentity_principalId :: Lens.Lens' IamIdentity (Prelude.Maybe Prelude.Text)
iamIdentity_principalId = Lens.lens (\IamIdentity' {principalId} -> principalId) (\s@IamIdentity' {} a -> s {principalId = a} :: IamIdentity)

-- | The person or application which assumes the IAM identity.
iamIdentity_sourceIdentity :: Lens.Lens' IamIdentity (Prelude.Maybe Prelude.Text)
iamIdentity_sourceIdentity = Lens.lens (\IamIdentity' {sourceIdentity} -> sourceIdentity) (\s@IamIdentity' {} a -> s {sourceIdentity = a} :: IamIdentity)

instance Data.FromJSON IamIdentity where
  parseJSON =
    Data.withObject
      "IamIdentity"
      ( \x ->
          IamIdentity'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "PrincipalId")
            Prelude.<*> (x Data..:? "SourceIdentity")
      )

instance Prelude.Hashable IamIdentity where
  hashWithSalt _salt IamIdentity' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` sourceIdentity

instance Prelude.NFData IamIdentity where
  rnf IamIdentity' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf sourceIdentity
