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
-- Module      : Amazonka.LexV2Models.Types.Principal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.Principal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The IAM principal that you allowing or denying access to an Amazon Lex
-- action. You must provide a @service@ or an @arn@, but not both in the
-- same statement. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html AWS JSON policy elements: Principal>
-- .
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The Amazon Resource Name (ARN) of the principal.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS service that should allowed or denied access to an
    -- Amazon Lex action.
    service :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'principal_arn' - The Amazon Resource Name (ARN) of the principal.
--
-- 'service', 'principal_service' - The name of the AWS service that should allowed or denied access to an
-- Amazon Lex action.
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { arn = Prelude.Nothing,
      service = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the principal.
principal_arn :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_arn = Lens.lens (\Principal' {arn} -> arn) (\s@Principal' {} a -> s {arn = a} :: Principal)

-- | The name of the AWS service that should allowed or denied access to an
-- Amazon Lex action.
principal_service :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_service = Lens.lens (\Principal' {service} -> service) (\s@Principal' {} a -> s {service = a} :: Principal)

instance Prelude.Hashable Principal where
  hashWithSalt _salt Principal' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` service

instance Prelude.NFData Principal where
  rnf Principal' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf service

instance Data.ToJSON Principal where
  toJSON Principal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arn" Data..=) Prelude.<$> arn,
            ("service" Data..=) Prelude.<$> service
          ]
      )
