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
-- Module      : Network.AWS.EMR.Types.SessionMappingSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SessionMappingSummary where

import Network.AWS.EMR.Types.IdentityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details for an Amazon EMR Studio session mapping. The details do not
-- include the time the session mapping was last modified.
--
-- /See:/ 'newSessionMappingSummary' smart constructor.
data SessionMappingSummary = SessionMappingSummary'
  { -- | The time the session mapping was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the user or group. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /AWS SSO Identity Store API Reference/.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
    -- or a group.
    identityType :: Prelude.Maybe IdentityType,
    -- | The globally unique identifier (GUID) of the user or group from the AWS
    -- SSO Identity Store.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the session policy associated with the
    -- user or group.
    sessionPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SessionMappingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'sessionMappingSummary_creationTime' - The time the session mapping was created.
--
-- 'identityName', 'sessionMappingSummary_identityName' - The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/.
--
-- 'identityType', 'sessionMappingSummary_identityType' - Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
--
-- 'identityId', 'sessionMappingSummary_identityId' - The globally unique identifier (GUID) of the user or group from the AWS
-- SSO Identity Store.
--
-- 'sessionPolicyArn', 'sessionMappingSummary_sessionPolicyArn' - The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
--
-- 'studioId', 'sessionMappingSummary_studioId' - The ID of the Amazon EMR Studio.
newSessionMappingSummary ::
  SessionMappingSummary
newSessionMappingSummary =
  SessionMappingSummary'
    { creationTime =
        Prelude.Nothing,
      identityName = Prelude.Nothing,
      identityType = Prelude.Nothing,
      identityId = Prelude.Nothing,
      sessionPolicyArn = Prelude.Nothing,
      studioId = Prelude.Nothing
    }

-- | The time the session mapping was created.
sessionMappingSummary_creationTime :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.UTCTime)
sessionMappingSummary_creationTime = Lens.lens (\SessionMappingSummary' {creationTime} -> creationTime) (\s@SessionMappingSummary' {} a -> s {creationTime = a} :: SessionMappingSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the user or group. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/.
sessionMappingSummary_identityName :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_identityName = Lens.lens (\SessionMappingSummary' {identityName} -> identityName) (\s@SessionMappingSummary' {} a -> s {identityName = a} :: SessionMappingSummary)

-- | Specifies whether the identity mapped to the Amazon EMR Studio is a user
-- or a group.
sessionMappingSummary_identityType :: Lens.Lens' SessionMappingSummary (Prelude.Maybe IdentityType)
sessionMappingSummary_identityType = Lens.lens (\SessionMappingSummary' {identityType} -> identityType) (\s@SessionMappingSummary' {} a -> s {identityType = a} :: SessionMappingSummary)

-- | The globally unique identifier (GUID) of the user or group from the AWS
-- SSO Identity Store.
sessionMappingSummary_identityId :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_identityId = Lens.lens (\SessionMappingSummary' {identityId} -> identityId) (\s@SessionMappingSummary' {} a -> s {identityId = a} :: SessionMappingSummary)

-- | The Amazon Resource Name (ARN) of the session policy associated with the
-- user or group.
sessionMappingSummary_sessionPolicyArn :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_sessionPolicyArn = Lens.lens (\SessionMappingSummary' {sessionPolicyArn} -> sessionPolicyArn) (\s@SessionMappingSummary' {} a -> s {sessionPolicyArn = a} :: SessionMappingSummary)

-- | The ID of the Amazon EMR Studio.
sessionMappingSummary_studioId :: Lens.Lens' SessionMappingSummary (Prelude.Maybe Prelude.Text)
sessionMappingSummary_studioId = Lens.lens (\SessionMappingSummary' {studioId} -> studioId) (\s@SessionMappingSummary' {} a -> s {studioId = a} :: SessionMappingSummary)

instance Prelude.FromJSON SessionMappingSummary where
  parseJSON =
    Prelude.withObject
      "SessionMappingSummary"
      ( \x ->
          SessionMappingSummary'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "IdentityName")
            Prelude.<*> (x Prelude..:? "IdentityType")
            Prelude.<*> (x Prelude..:? "IdentityId")
            Prelude.<*> (x Prelude..:? "SessionPolicyArn")
            Prelude.<*> (x Prelude..:? "StudioId")
      )

instance Prelude.Hashable SessionMappingSummary

instance Prelude.NFData SessionMappingSummary
