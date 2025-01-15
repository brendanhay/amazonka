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
-- Module      : Amazonka.Kendra.Types.EntityPersonaConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.EntityPersonaConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.Persona
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for users or groups in your IAM
-- Identity Center identity source for access to your Amazon Kendra
-- experience. Specific permissions are defined for each user or group once
-- they are granted access to your Amazon Kendra experience.
--
-- /See:/ 'newEntityPersonaConfiguration' smart constructor.
data EntityPersonaConfiguration = EntityPersonaConfiguration'
  { -- | The identifier of a user or group in your IAM Identity Center identity
    -- source. For example, a user ID could be an email.
    entityId :: Prelude.Text,
    -- | The persona that defines the specific permissions of the user or group
    -- in your IAM Identity Center identity source. The available personas or
    -- access roles are @Owner@ and @Viewer@. For more information on these
    -- personas, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html#access-search-experience Providing access to your search page>.
    persona :: Persona
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityPersonaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'entityPersonaConfiguration_entityId' - The identifier of a user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
--
-- 'persona', 'entityPersonaConfiguration_persona' - The persona that defines the specific permissions of the user or group
-- in your IAM Identity Center identity source. The available personas or
-- access roles are @Owner@ and @Viewer@. For more information on these
-- personas, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html#access-search-experience Providing access to your search page>.
newEntityPersonaConfiguration ::
  -- | 'entityId'
  Prelude.Text ->
  -- | 'persona'
  Persona ->
  EntityPersonaConfiguration
newEntityPersonaConfiguration pEntityId_ pPersona_ =
  EntityPersonaConfiguration'
    { entityId = pEntityId_,
      persona = pPersona_
    }

-- | The identifier of a user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
entityPersonaConfiguration_entityId :: Lens.Lens' EntityPersonaConfiguration Prelude.Text
entityPersonaConfiguration_entityId = Lens.lens (\EntityPersonaConfiguration' {entityId} -> entityId) (\s@EntityPersonaConfiguration' {} a -> s {entityId = a} :: EntityPersonaConfiguration)

-- | The persona that defines the specific permissions of the user or group
-- in your IAM Identity Center identity source. The available personas or
-- access roles are @Owner@ and @Viewer@. For more information on these
-- personas, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html#access-search-experience Providing access to your search page>.
entityPersonaConfiguration_persona :: Lens.Lens' EntityPersonaConfiguration Persona
entityPersonaConfiguration_persona = Lens.lens (\EntityPersonaConfiguration' {persona} -> persona) (\s@EntityPersonaConfiguration' {} a -> s {persona = a} :: EntityPersonaConfiguration)

instance Prelude.Hashable EntityPersonaConfiguration where
  hashWithSalt _salt EntityPersonaConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` persona

instance Prelude.NFData EntityPersonaConfiguration where
  rnf EntityPersonaConfiguration' {..} =
    Prelude.rnf entityId `Prelude.seq`
      Prelude.rnf persona

instance Data.ToJSON EntityPersonaConfiguration where
  toJSON EntityPersonaConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EntityId" Data..= entityId),
            Prelude.Just ("Persona" Data..= persona)
          ]
      )
