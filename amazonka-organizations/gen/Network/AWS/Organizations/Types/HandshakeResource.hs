{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Organizations.Types.HandshakeResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeResource where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.HandshakeResourceType
import qualified Network.AWS.Prelude as Prelude

-- | Contains additional data that is needed to process a handshake.
--
-- /See:/ 'newHandshakeResource' smart constructor.
data HandshakeResource = HandshakeResource'
  { -- | When needed, contains an additional array of @HandshakeResource@
    -- objects.
    resources :: Prelude.Maybe [HandshakeResource],
    -- | The information that is passed to the other party in the handshake. The
    -- format of the value string must match the requirements of the specified
    -- type.
    value :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The type of information being passed, specifying how the value is to be
    -- interpreted by the other party:
    --
    -- -   @ACCOUNT@ - Specifies an AWS account ID number.
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
    type' :: Prelude.Maybe HandshakeResourceType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'value', 'handshakeResource_value' - The information that is passed to the other party in the handshake. The
-- format of the value string must match the requirements of the specified
-- type.
--
-- 'type'', 'handshakeResource_type' - The type of information being passed, specifying how the value is to be
-- interpreted by the other party:
--
-- -   @ACCOUNT@ - Specifies an AWS account ID number.
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
newHandshakeResource ::
  HandshakeResource
newHandshakeResource =
  HandshakeResource'
    { resources = Prelude.Nothing,
      value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | When needed, contains an additional array of @HandshakeResource@
-- objects.
handshakeResource_resources :: Lens.Lens' HandshakeResource (Prelude.Maybe [HandshakeResource])
handshakeResource_resources = Lens.lens (\HandshakeResource' {resources} -> resources) (\s@HandshakeResource' {} a -> s {resources = a} :: HandshakeResource) Prelude.. Lens.mapping Prelude._Coerce

-- | The information that is passed to the other party in the handshake. The
-- format of the value string must match the requirements of the specified
-- type.
handshakeResource_value :: Lens.Lens' HandshakeResource (Prelude.Maybe Prelude.Text)
handshakeResource_value = Lens.lens (\HandshakeResource' {value} -> value) (\s@HandshakeResource' {} a -> s {value = a} :: HandshakeResource) Prelude.. Lens.mapping Prelude._Sensitive

-- | The type of information being passed, specifying how the value is to be
-- interpreted by the other party:
--
-- -   @ACCOUNT@ - Specifies an AWS account ID number.
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

instance Prelude.FromJSON HandshakeResource where
  parseJSON =
    Prelude.withObject
      "HandshakeResource"
      ( \x ->
          HandshakeResource'
            Prelude.<$> ( x Prelude..:? "Resources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable HandshakeResource

instance Prelude.NFData HandshakeResource
