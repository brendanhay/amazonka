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
-- Module      : Amazonka.Nimble.Types.Studio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.Studio where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.StudioEncryptionConfiguration
import Amazonka.Nimble.Types.StudioState
import Amazonka.Nimble.Types.StudioStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Represents a studio resource.
--
-- A studio is the core resource used with Nimble Studio. You must create a
-- studio first, before any other resource type can be created. All other
-- resources you create and manage in Nimble Studio are contained within a
-- studio.
--
-- When creating a studio, you must provides two IAM roles for use with the
-- Nimble Studio portal. These roles are assumed by your users when they
-- log in to the Nimble Studio portal via IAM Identity Center and your
-- identity source.
--
-- The user role must have the AmazonNimbleStudio-StudioUser managed policy
-- attached for the portal to function properly.
--
-- The admin role must have the AmazonNimbleStudio-StudioAdmin managed
-- policy attached for the portal to function properly.
--
-- Your studio roles must trust the identity.nimble.amazonaws.com service
-- principal to function properly.
--
-- /See:/ 'newStudio' smart constructor.
data Studio = Studio'
  { -- | The unique identifier for a studio resource. In Nimble Studio, all other
    -- resources are contained in a studio resource.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the studio, as included in the URL when accessing it in the
    -- Nimble Studio portal.
    studioName :: Prelude.Maybe Prelude.Text,
    -- | The IAM Identity Center application client ID used to integrate with IAM
    -- Identity Center to enable IAM Identity Center users to log in to Nimble
    -- Studio portal.
    ssoClientId :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that studio admins assume when logging in to the Nimble
    -- Studio portal.
    adminRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
    -- uniquely identifies it. ARNs are unique across all Regions.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A friendly name for the studio.
    displayName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The current state of the studio resource.
    state :: Prelude.Maybe StudioState,
    -- | Configuration of the encryption method that is used for the studio.
    studioEncryptionConfiguration :: Prelude.Maybe StudioEncryptionConfiguration,
    -- | The address of the web page for the studio.
    studioUrl :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that studio users assume when logging in to the Nimble
    -- Studio portal.
    userRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the studio resource is located.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | Status codes that provide additional detail on the studio state.
    statusCode :: Prelude.Maybe StudioStatusCode,
    -- | Additional detail on the studio state.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Studio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'studio_studioId' - The unique identifier for a studio resource. In Nimble Studio, all other
-- resources are contained in a studio resource.
--
-- 'tags', 'studio_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'studioName', 'studio_studioName' - The name of the studio, as included in the URL when accessing it in the
-- Nimble Studio portal.
--
-- 'ssoClientId', 'studio_ssoClientId' - The IAM Identity Center application client ID used to integrate with IAM
-- Identity Center to enable IAM Identity Center users to log in to Nimble
-- Studio portal.
--
-- 'adminRoleArn', 'studio_adminRoleArn' - The IAM role that studio admins assume when logging in to the Nimble
-- Studio portal.
--
-- 'arn', 'studio_arn' - The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
--
-- 'displayName', 'studio_displayName' - A friendly name for the studio.
--
-- 'state', 'studio_state' - The current state of the studio resource.
--
-- 'studioEncryptionConfiguration', 'studio_studioEncryptionConfiguration' - Configuration of the encryption method that is used for the studio.
--
-- 'studioUrl', 'studio_studioUrl' - The address of the web page for the studio.
--
-- 'userRoleArn', 'studio_userRoleArn' - The IAM role that studio users assume when logging in to the Nimble
-- Studio portal.
--
-- 'homeRegion', 'studio_homeRegion' - The Amazon Web Services Region where the studio resource is located.
--
-- 'statusCode', 'studio_statusCode' - Status codes that provide additional detail on the studio state.
--
-- 'statusMessage', 'studio_statusMessage' - Additional detail on the studio state.
--
-- 'createdAt', 'studio_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'updatedAt', 'studio_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
newStudio ::
  Studio
newStudio =
  Studio'
    { studioId = Prelude.Nothing,
      tags = Prelude.Nothing,
      studioName = Prelude.Nothing,
      ssoClientId = Prelude.Nothing,
      adminRoleArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      state = Prelude.Nothing,
      studioEncryptionConfiguration = Prelude.Nothing,
      studioUrl = Prelude.Nothing,
      userRoleArn = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The unique identifier for a studio resource. In Nimble Studio, all other
-- resources are contained in a studio resource.
studio_studioId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioId = Lens.lens (\Studio' {studioId} -> studioId) (\s@Studio' {} a -> s {studioId = a} :: Studio)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
studio_tags :: Lens.Lens' Studio (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
studio_tags = Lens.lens (\Studio' {tags} -> tags) (\s@Studio' {} a -> s {tags = a} :: Studio) Prelude.. Lens.mapping Lens.coerced

-- | The name of the studio, as included in the URL when accessing it in the
-- Nimble Studio portal.
studio_studioName :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioName = Lens.lens (\Studio' {studioName} -> studioName) (\s@Studio' {} a -> s {studioName = a} :: Studio)

-- | The IAM Identity Center application client ID used to integrate with IAM
-- Identity Center to enable IAM Identity Center users to log in to Nimble
-- Studio portal.
studio_ssoClientId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_ssoClientId = Lens.lens (\Studio' {ssoClientId} -> ssoClientId) (\s@Studio' {} a -> s {ssoClientId = a} :: Studio)

-- | The IAM role that studio admins assume when logging in to the Nimble
-- Studio portal.
studio_adminRoleArn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_adminRoleArn = Lens.lens (\Studio' {adminRoleArn} -> adminRoleArn) (\s@Studio' {} a -> s {adminRoleArn = a} :: Studio)

-- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
studio_arn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_arn = Lens.lens (\Studio' {arn} -> arn) (\s@Studio' {} a -> s {arn = a} :: Studio)

-- | A friendly name for the studio.
studio_displayName :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_displayName = Lens.lens (\Studio' {displayName} -> displayName) (\s@Studio' {} a -> s {displayName = a} :: Studio) Prelude.. Lens.mapping Core._Sensitive

-- | The current state of the studio resource.
studio_state :: Lens.Lens' Studio (Prelude.Maybe StudioState)
studio_state = Lens.lens (\Studio' {state} -> state) (\s@Studio' {} a -> s {state = a} :: Studio)

-- | Configuration of the encryption method that is used for the studio.
studio_studioEncryptionConfiguration :: Lens.Lens' Studio (Prelude.Maybe StudioEncryptionConfiguration)
studio_studioEncryptionConfiguration = Lens.lens (\Studio' {studioEncryptionConfiguration} -> studioEncryptionConfiguration) (\s@Studio' {} a -> s {studioEncryptionConfiguration = a} :: Studio)

-- | The address of the web page for the studio.
studio_studioUrl :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioUrl = Lens.lens (\Studio' {studioUrl} -> studioUrl) (\s@Studio' {} a -> s {studioUrl = a} :: Studio)

-- | The IAM role that studio users assume when logging in to the Nimble
-- Studio portal.
studio_userRoleArn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_userRoleArn = Lens.lens (\Studio' {userRoleArn} -> userRoleArn) (\s@Studio' {} a -> s {userRoleArn = a} :: Studio)

-- | The Amazon Web Services Region where the studio resource is located.
studio_homeRegion :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_homeRegion = Lens.lens (\Studio' {homeRegion} -> homeRegion) (\s@Studio' {} a -> s {homeRegion = a} :: Studio)

-- | Status codes that provide additional detail on the studio state.
studio_statusCode :: Lens.Lens' Studio (Prelude.Maybe StudioStatusCode)
studio_statusCode = Lens.lens (\Studio' {statusCode} -> statusCode) (\s@Studio' {} a -> s {statusCode = a} :: Studio)

-- | Additional detail on the studio state.
studio_statusMessage :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_statusMessage = Lens.lens (\Studio' {statusMessage} -> statusMessage) (\s@Studio' {} a -> s {statusMessage = a} :: Studio)

-- | The Unix epoch timestamp in seconds for when the resource was created.
studio_createdAt :: Lens.Lens' Studio (Prelude.Maybe Prelude.UTCTime)
studio_createdAt = Lens.lens (\Studio' {createdAt} -> createdAt) (\s@Studio' {} a -> s {createdAt = a} :: Studio) Prelude.. Lens.mapping Core._Time

-- | The Unix epoch timestamp in seconds for when the resource was updated.
studio_updatedAt :: Lens.Lens' Studio (Prelude.Maybe Prelude.UTCTime)
studio_updatedAt = Lens.lens (\Studio' {updatedAt} -> updatedAt) (\s@Studio' {} a -> s {updatedAt = a} :: Studio) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Studio where
  parseJSON =
    Core.withObject
      "Studio"
      ( \x ->
          Studio'
            Prelude.<$> (x Core..:? "studioId")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "studioName")
            Prelude.<*> (x Core..:? "ssoClientId")
            Prelude.<*> (x Core..:? "adminRoleArn")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "studioEncryptionConfiguration")
            Prelude.<*> (x Core..:? "studioUrl")
            Prelude.<*> (x Core..:? "userRoleArn")
            Prelude.<*> (x Core..:? "homeRegion")
            Prelude.<*> (x Core..:? "statusCode")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "updatedAt")
      )

instance Prelude.Hashable Studio where
  hashWithSalt _salt Studio' {..} =
    _salt `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` studioName
      `Prelude.hashWithSalt` ssoClientId
      `Prelude.hashWithSalt` adminRoleArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` studioEncryptionConfiguration
      `Prelude.hashWithSalt` studioUrl
      `Prelude.hashWithSalt` userRoleArn
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Studio where
  rnf Studio' {..} =
    Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf studioName
      `Prelude.seq` Prelude.rnf ssoClientId
      `Prelude.seq` Prelude.rnf adminRoleArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf studioEncryptionConfiguration
      `Prelude.seq` Prelude.rnf studioUrl
      `Prelude.seq` Prelude.rnf userRoleArn
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
