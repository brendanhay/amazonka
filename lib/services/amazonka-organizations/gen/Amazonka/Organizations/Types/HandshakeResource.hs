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
-- Module      : Amazonka.Organizations.Types.HandshakeResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.HandshakeResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types.HandshakeResourceType
import qualified Amazonka.Prelude as Prelude

-- | Contains additional data that is needed to process a handshake.
--
-- /See:/ 'newHandshakeResource' smart constructor.
data HandshakeResource = HandshakeResource'
  { -- | When needed, contains an additional array of @HandshakeResource@
    -- objects.
    resources :: Prelude.Maybe [HandshakeResource],
    -- | The type of information being passed, specifying how the value is to be
    -- interpreted by the other party:
    --
    -- -   @ACCOUNT@ - Specifies an Amazon Web Services account ID number.
    --
    -- -   @ORGANIZATION@ - Specifies an organization ID number.
    --
    -- -   @EMAIL@ - Specifies the email address that is associated with the
    --     account that receives the handshake.
    --
    -- -   @OWNER_EMAIL@ - Specifies the email address associated with the
    --     management account. Included as information about an organization.
    --
    -- -   @OWNER_NAME@ - Specifies the name associated with the management
    --     account. Included as information about an organization.
    --
    -- -   @NOTES@ - Additional text provided by the handshake initiator and
    --     intended for the recipient to read.
    type' :: Prelude.Maybe HandshakeResourceType,
    -- | The information that is passed to the other party in the handshake. The
    -- format of the value string must match the requirements of the specified
    -- type.
    value :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HandshakeResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'handshakeResource_resources' - When needed, contains an additional array of @HandshakeResource@
-- objects.
--
-- 'type'', 'handshakeResource_type' - The type of information being passed, specifying how the value is to be
-- interpreted by the other party:
--
-- -   @ACCOUNT@ - Specifies an Amazon Web Services account ID number.
--
-- -   @ORGANIZATION@ - Specifies an organization ID number.
--
-- -   @EMAIL@ - Specifies the email address that is associated with the
--     account that receives the handshake.
--
-- -   @OWNER_EMAIL@ - Specifies the email address associated with the
--     management account. Included as information about an organization.
--
-- -   @OWNER_NAME@ - Specifies the name associated with the management
--     account. Included as information about an organization.
--
-- -   @NOTES@ - Additional text provided by the handshake initiator and
--     intended for the recipient to read.
--
-- 'value', 'handshakeResource_value' - The information that is passed to the other party in the handshake. The
-- format of the value string must match the requirements of the specified
-- type.
newHandshakeResource ::
  HandshakeResource
newHandshakeResource =
  HandshakeResource'
    { resources = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | When needed, contains an additional array of @HandshakeResource@
-- objects.
handshakeResource_resources :: Lens.Lens' HandshakeResource (Prelude.Maybe [HandshakeResource])
handshakeResource_resources = Lens.lens (\HandshakeResource' {resources} -> resources) (\s@HandshakeResource' {} a -> s {resources = a} :: HandshakeResource) Prelude.. Lens.mapping Lens.coerced

-- | The type of information being passed, specifying how the value is to be
-- interpreted by the other party:
--
-- -   @ACCOUNT@ - Specifies an Amazon Web Services account ID number.
--
-- -   @ORGANIZATION@ - Specifies an organization ID number.
--
-- -   @EMAIL@ - Specifies the email address that is associated with the
--     account that receives the handshake.
--
-- -   @OWNER_EMAIL@ - Specifies the email address associated with the
--     management account. Included as information about an organization.
--
-- -   @OWNER_NAME@ - Specifies the name associated with the management
--     account. Included as information about an organization.
--
-- -   @NOTES@ - Additional text provided by the handshake initiator and
--     intended for the recipient to read.
handshakeResource_type :: Lens.Lens' HandshakeResource (Prelude.Maybe HandshakeResourceType)
handshakeResource_type = Lens.lens (\HandshakeResource' {type'} -> type') (\s@HandshakeResource' {} a -> s {type' = a} :: HandshakeResource)

-- | The information that is passed to the other party in the handshake. The
-- format of the value string must match the requirements of the specified
-- type.
handshakeResource_value :: Lens.Lens' HandshakeResource (Prelude.Maybe Prelude.Text)
handshakeResource_value = Lens.lens (\HandshakeResource' {value} -> value) (\s@HandshakeResource' {} a -> s {value = a} :: HandshakeResource) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON HandshakeResource where
  parseJSON =
    Data.withObject
      "HandshakeResource"
      ( \x ->
          HandshakeResource'
            Prelude.<$> (x Data..:? "Resources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable HandshakeResource where
  hashWithSalt _salt HandshakeResource' {..} =
    _salt `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData HandshakeResource where
  rnf HandshakeResource' {..} =
    Prelude.rnf resources
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value
