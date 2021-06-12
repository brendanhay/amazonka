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
-- Module      : Network.AWS.EMR.Types.SessionMappingDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SessionMappingDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.IdentityType
import qualified Network.AWS.Lens as Lens

-- | Details for an Amazon EMR Studio session mapping including creation
-- time, user or group ID, Studio ID, and so on.
--
-- /See:/ 'newSessionMappingDetail' smart constructor.
data SessionMappingDetail = SessionMappingDetail'
  { -- | The time the session mapping was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the user or group. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /AWS SSO Identity Store API Reference/.
    identityName :: Core.Maybe Core.Text,
    -- | The time the session mapping was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
    -- or a group.
    identityType :: Core.Maybe IdentityType,
    -- | The globally unique identifier (GUID) of the user or group.
    identityId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the session policy associated with the
    -- user or group.
    sessionPolicyArn :: Core.Maybe Core.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'identityName', 'sessionMappingDetail_identityName' - The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/.
--
-- 'lastModifiedTime', 'sessionMappingDetail_lastModifiedTime' - The time the session mapping was last modified.
--
-- 'identityType', 'sessionMappingDetail_identityType' - Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
--
-- 'identityId', 'sessionMappingDetail_identityId' - The globally unique identifier (GUID) of the user or group.
--
-- 'sessionPolicyArn', 'sessionMappingDetail_sessionPolicyArn' - The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
--
-- 'studioId', 'sessionMappingDetail_studioId' - The ID of the Amazon EMR Studio.
newSessionMappingDetail ::
  SessionMappingDetail
newSessionMappingDetail =
  SessionMappingDetail'
    { creationTime = Core.Nothing,
      identityName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      identityType = Core.Nothing,
      identityId = Core.Nothing,
      sessionPolicyArn = Core.Nothing,
      studioId = Core.Nothing
    }

-- | The time the session mapping was created.
sessionMappingDetail_creationTime :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.UTCTime)
sessionMappingDetail_creationTime = Lens.lens (\SessionMappingDetail' {creationTime} -> creationTime) (\s@SessionMappingDetail' {} a -> s {creationTime = a} :: SessionMappingDetail) Core.. Lens.mapping Core._Time

-- | The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/.
sessionMappingDetail_identityName :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.Text)
sessionMappingDetail_identityName = Lens.lens (\SessionMappingDetail' {identityName} -> identityName) (\s@SessionMappingDetail' {} a -> s {identityName = a} :: SessionMappingDetail)

-- | The time the session mapping was last modified.
sessionMappingDetail_lastModifiedTime :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.UTCTime)
sessionMappingDetail_lastModifiedTime = Lens.lens (\SessionMappingDetail' {lastModifiedTime} -> lastModifiedTime) (\s@SessionMappingDetail' {} a -> s {lastModifiedTime = a} :: SessionMappingDetail) Core.. Lens.mapping Core._Time

-- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
sessionMappingDetail_identityType :: Lens.Lens' SessionMappingDetail (Core.Maybe IdentityType)
sessionMappingDetail_identityType = Lens.lens (\SessionMappingDetail' {identityType} -> identityType) (\s@SessionMappingDetail' {} a -> s {identityType = a} :: SessionMappingDetail)

-- | The globally unique identifier (GUID) of the user or group.
sessionMappingDetail_identityId :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.Text)
sessionMappingDetail_identityId = Lens.lens (\SessionMappingDetail' {identityId} -> identityId) (\s@SessionMappingDetail' {} a -> s {identityId = a} :: SessionMappingDetail)

-- | The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
sessionMappingDetail_sessionPolicyArn :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.Text)
sessionMappingDetail_sessionPolicyArn = Lens.lens (\SessionMappingDetail' {sessionPolicyArn} -> sessionPolicyArn) (\s@SessionMappingDetail' {} a -> s {sessionPolicyArn = a} :: SessionMappingDetail)

-- | The ID of the Amazon EMR Studio.
sessionMappingDetail_studioId :: Lens.Lens' SessionMappingDetail (Core.Maybe Core.Text)
sessionMappingDetail_studioId = Lens.lens (\SessionMappingDetail' {studioId} -> studioId) (\s@SessionMappingDetail' {} a -> s {studioId = a} :: SessionMappingDetail)

instance Core.FromJSON SessionMappingDetail where
  parseJSON =
    Core.withObject
      "SessionMappingDetail"
      ( \x ->
          SessionMappingDetail'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "IdentityName")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "IdentityType")
            Core.<*> (x Core..:? "IdentityId")
            Core.<*> (x Core..:? "SessionPolicyArn")
            Core.<*> (x Core..:? "StudioId")
      )

instance Core.Hashable SessionMappingDetail

instance Core.NFData SessionMappingDetail
