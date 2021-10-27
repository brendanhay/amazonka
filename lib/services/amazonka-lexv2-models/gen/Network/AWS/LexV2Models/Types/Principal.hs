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
-- Module      : Network.AWS.LexV2Models.Types.Principal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.Principal where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Prelude.Hashable Principal

instance Prelude.NFData Principal

instance Core.ToJSON Principal where
  toJSON Principal' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("arn" Core..=) Prelude.<$> arn,
            ("service" Core..=) Prelude.<$> service
          ]
      )
