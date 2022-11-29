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
-- Module      : Amazonka.EMR.Types.SessionMappingSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SessionMappingSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.IdentityType
import qualified Amazonka.Prelude as Prelude

-- | Details for an Amazon EMR Studio session mapping. The details do not
-- include the time the session mapping was last modified.
--
-- /See:/ 'newSessionMappingSummary' smart constructor.
data SessionMappingSummary = SessionMappingSummary'
  { -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the session policy associated with the
    -- user or group.
    sessionPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the user or group. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /Amazon Web Services SSO Identity Store API Reference/.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The time the session mapping was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The globally unique identifier (GUID) of the user or group from the
    -- Amazon Web Services SSO Identity Store.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
    -- or a group.
    identityType :: Prelude.Maybe IdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionMappingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'sessionMappingSummary_studioId' - The ID of the Amazon EMR Studio.
--
-- 'sessionPolicyArn', 'sessionMappingSummary_sessionPolicyArn' - The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
--
-- 'identityName', 'sessionMappingSummary_identityName' - The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/.
--
-- 'creationTime', 'sessionMappingSummary_creationTime' - The time the session mapping was created.
--
-- 'identityId', 'sessionMappingSummary_identityId' - The globally unique identifier (GUID) of the user or group from the
-- Amazon Web Services SSO Identity Store.
--
-- 'identityType', 'sessionMappingSummary_identityType' - Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
newSessionMappingSummary ::
  SessionMappingSummary
newSessionMappingSummary =
  SessionMappingSummary'
    { studioId = Prelude.Nothing,
      sessionPolicyArn = Prelude.Nothing,
      identityName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      identityId = Prelude.Nothing,
      identityType = Prelude.Nothing
    }

-- | The ID of the Amazon EMR Studio.
sessionMappingSummary_studioId :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_studioId = Lens.lens (\SessionMappingSummary' {studioId} -> studioId) (\s@SessionMappingSummary' {} a -> s {studioId = a} :: SessionMappingSummary)

-- | The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
sessionMappingSummary_sessionPolicyArn :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_sessionPolicyArn = Lens.lens (\SessionMappingSummary' {sessionPolicyArn} -> sessionPolicyArn) (\s@SessionMappingSummary' {} a -> s {sessionPolicyArn = a} :: SessionMappingSummary)

-- | The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/.
sessionMappingSummary_identityName :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_identityName = Lens.lens (\SessionMappingSummary' {identityName} -> identityName) (\s@SessionMappingSummary' {} a -> s {identityName = a} :: SessionMappingSummary)

-- | The time the session mapping was created.
sessionMappingSummary_creationTime :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.UTCTime)
sessionMappingSummary_creationTime = Lens.lens (\SessionMappingSummary' {creationTime} -> creationTime) (\s@SessionMappingSummary' {} a -> s {creationTime = a} :: SessionMappingSummary) Prelude.. Lens.mapping Core._Time

-- | The globally unique identifier (GUID) of the user or group from the
-- Amazon Web Services SSO Identity Store.
sessionMappingSummary_identityId :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_identityId = Lens.lens (\SessionMappingSummary' {identityId} -> identityId) (\s@SessionMappingSummary' {} a -> s {identityId = a} :: SessionMappingSummary)

-- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
sessionMappingSummary_identityType :: Lens.Lens' SessionMappingSummary (Prelude.Maybe IdentityType)
sessionMappingSummary_identityType = Lens.lens (\SessionMappingSummary' {identityType} -> identityType) (\s@SessionMappingSummary' {} a -> s {identityType = a} :: SessionMappingSummary)

instance Core.FromJSON SessionMappingSummary where
  parseJSON =
    Core.withObject
      "SessionMappingSummary"
      ( \x ->
          SessionMappingSummary'
            Prelude.<$> (x Core..:? "StudioId")
            Prelude.<*> (x Core..:? "SessionPolicyArn")
            Prelude.<*> (x Core..:? "IdentityName")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "IdentityId")
            Prelude.<*> (x Core..:? "IdentityType")
      )

instance Prelude.Hashable SessionMappingSummary where
  hashWithSalt _salt SessionMappingSummary' {..} =
    _salt `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` sessionPolicyArn
      `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` identityType

instance Prelude.NFData SessionMappingSummary where
  rnf SessionMappingSummary' {..} =
    Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf sessionPolicyArn
      `Prelude.seq` Prelude.rnf identityName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf identityType
