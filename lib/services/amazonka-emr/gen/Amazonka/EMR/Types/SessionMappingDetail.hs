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
-- Module      : Amazonka.EMR.Types.SessionMappingDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SessionMappingDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.IdentityType
import qualified Amazonka.Prelude as Prelude

-- | Details for an Amazon EMR Studio session mapping including creation
-- time, user or group ID, Studio ID, and so on.
--
-- /See:/ 'newSessionMappingDetail' smart constructor.
data SessionMappingDetail = SessionMappingDetail'
  { -- | The time the session mapping was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The globally unique identifier (GUID) of the user or group.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user or group. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /Amazon Web Services SSO Identity Store API Reference/.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
    -- or a group.
    identityType :: Prelude.Maybe IdentityType,
    -- | The time the session mapping was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the session policy associated with the
    -- user or group.
    sessionPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionMappingDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'sessionMappingDetail_creationTime' - The time the session mapping was created.
--
-- 'identityId', 'sessionMappingDetail_identityId' - The globally unique identifier (GUID) of the user or group.
--
-- 'identityName', 'sessionMappingDetail_identityName' - The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/.
--
-- 'identityType', 'sessionMappingDetail_identityType' - Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
--
-- 'lastModifiedTime', 'sessionMappingDetail_lastModifiedTime' - The time the session mapping was last modified.
--
-- 'sessionPolicyArn', 'sessionMappingDetail_sessionPolicyArn' - The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
--
-- 'studioId', 'sessionMappingDetail_studioId' - The ID of the Amazon EMR Studio.
newSessionMappingDetail ::
  SessionMappingDetail
newSessionMappingDetail =
  SessionMappingDetail'
    { creationTime =
        Prelude.Nothing,
      identityId = Prelude.Nothing,
      identityName = Prelude.Nothing,
      identityType = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      sessionPolicyArn = Prelude.Nothing,
      studioId = Prelude.Nothing
    }

-- | The time the session mapping was created.
sessionMappingDetail_creationTime :: Lens.Lens' SessionMappingDetail (Prelude.Maybe Prelude.UTCTime)
sessionMappingDetail_creationTime = Lens.lens (\SessionMappingDetail' {creationTime} -> creationTime) (\s@SessionMappingDetail' {} a -> s {creationTime = a} :: SessionMappingDetail) Prelude.. Lens.mapping Data._Time

-- | The globally unique identifier (GUID) of the user or group.
sessionMappingDetail_identityId :: Lens.Lens' SessionMappingDetail (Prelude.Maybe Prelude.Text)
sessionMappingDetail_identityId = Lens.lens (\SessionMappingDetail' {identityId} -> identityId) (\s@SessionMappingDetail' {} a -> s {identityId = a} :: SessionMappingDetail)

-- | The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/.
sessionMappingDetail_identityName :: Lens.Lens' SessionMappingDetail (Prelude.Maybe Prelude.Text)
sessionMappingDetail_identityName = Lens.lens (\SessionMappingDetail' {identityName} -> identityName) (\s@SessionMappingDetail' {} a -> s {identityName = a} :: SessionMappingDetail)

-- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
sessionMappingDetail_identityType :: Lens.Lens' SessionMappingDetail (Prelude.Maybe IdentityType)
sessionMappingDetail_identityType = Lens.lens (\SessionMappingDetail' {identityType} -> identityType) (\s@SessionMappingDetail' {} a -> s {identityType = a} :: SessionMappingDetail)

-- | The time the session mapping was last modified.
sessionMappingDetail_lastModifiedTime :: Lens.Lens' SessionMappingDetail (Prelude.Maybe Prelude.UTCTime)
sessionMappingDetail_lastModifiedTime = Lens.lens (\SessionMappingDetail' {lastModifiedTime} -> lastModifiedTime) (\s@SessionMappingDetail' {} a -> s {lastModifiedTime = a} :: SessionMappingDetail) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
sessionMappingDetail_sessionPolicyArn :: Lens.Lens' SessionMappingDetail (Prelude.Maybe Prelude.Text)
sessionMappingDetail_sessionPolicyArn = Lens.lens (\SessionMappingDetail' {sessionPolicyArn} -> sessionPolicyArn) (\s@SessionMappingDetail' {} a -> s {sessionPolicyArn = a} :: SessionMappingDetail)

-- | The ID of the Amazon EMR Studio.
sessionMappingDetail_studioId :: Lens.Lens' SessionMappingDetail (Prelude.Maybe Prelude.Text)
sessionMappingDetail_studioId = Lens.lens (\SessionMappingDetail' {studioId} -> studioId) (\s@SessionMappingDetail' {} a -> s {studioId = a} :: SessionMappingDetail)

instance Data.FromJSON SessionMappingDetail where
  parseJSON =
    Data.withObject
      "SessionMappingDetail"
      ( \x ->
          SessionMappingDetail'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "IdentityId")
            Prelude.<*> (x Data..:? "IdentityName")
            Prelude.<*> (x Data..:? "IdentityType")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "SessionPolicyArn")
            Prelude.<*> (x Data..:? "StudioId")
      )

instance Prelude.Hashable SessionMappingDetail where
  hashWithSalt _salt SessionMappingDetail' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` sessionPolicyArn
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData SessionMappingDetail where
  rnf SessionMappingDetail' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf identityName
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf sessionPolicyArn
      `Prelude.seq` Prelude.rnf studioId
