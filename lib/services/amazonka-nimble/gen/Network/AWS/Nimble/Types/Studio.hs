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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.Studio where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.StudioEncryptionConfiguration
import Amazonka.Nimble.Types.StudioState
import Amazonka.Nimble.Types.StudioStatusCode
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newStudio' smart constructor.
data Studio = Studio'
  { -- | Configuration of the encryption method that is used for the studio.
    studioEncryptionConfiguration :: Prelude.Maybe StudioEncryptionConfiguration,
    -- | The current state of the studio resource.
    state :: Prelude.Maybe StudioState,
    -- | The name of the studio, as included in the URL when accessing it in the
    -- Nimble Studio portal.
    studioName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
    -- uniquely identifies it. ARNs are unique across all Regions.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for a studio resource. In Nimble Studio, all other
    -- resources are contained in a studio resource.
    studioId :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that studio users assume when logging in to the Nimble
    -- Studio portal.
    userRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services SSO application client ID used to integrate with
    -- Amazon Web Services SSO to enable Amazon Web Services SSO users to log
    -- in to Nimble portal.
    ssoClientId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the studio resource is located.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | Additional detail on the studio state.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | A friendly name for the studio.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The address of the web page for the studio.
    studioUrl :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that studio admins assume when logging in to the Nimble
    -- Studio portal.
    adminRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Status codes that provide additional detail on the studio state.
    statusCode :: Prelude.Maybe StudioStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Studio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioEncryptionConfiguration', 'studio_studioEncryptionConfiguration' - Configuration of the encryption method that is used for the studio.
--
-- 'state', 'studio_state' - The current state of the studio resource.
--
-- 'studioName', 'studio_studioName' - The name of the studio, as included in the URL when accessing it in the
-- Nimble Studio portal.
--
-- 'arn', 'studio_arn' - The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
--
-- 'createdAt', 'studio_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'studioId', 'studio_studioId' - The unique identifier for a studio resource. In Nimble Studio, all other
-- resources are contained in a studio resource.
--
-- 'userRoleArn', 'studio_userRoleArn' - The IAM role that studio users assume when logging in to the Nimble
-- Studio portal.
--
-- 'ssoClientId', 'studio_ssoClientId' - The Amazon Web Services SSO application client ID used to integrate with
-- Amazon Web Services SSO to enable Amazon Web Services SSO users to log
-- in to Nimble portal.
--
-- 'homeRegion', 'studio_homeRegion' - The Amazon Web Services Region where the studio resource is located.
--
-- 'statusMessage', 'studio_statusMessage' - Additional detail on the studio state.
--
-- 'displayName', 'studio_displayName' - A friendly name for the studio.
--
-- 'updatedAt', 'studio_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
--
-- 'studioUrl', 'studio_studioUrl' - The address of the web page for the studio.
--
-- 'adminRoleArn', 'studio_adminRoleArn' - The IAM role that studio admins assume when logging in to the Nimble
-- Studio portal.
--
-- 'tags', 'studio_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'statusCode', 'studio_statusCode' - Status codes that provide additional detail on the studio state.
newStudio ::
  Studio
newStudio =
  Studio'
    { studioEncryptionConfiguration =
        Prelude.Nothing,
      state = Prelude.Nothing,
      studioName = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      studioId = Prelude.Nothing,
      userRoleArn = Prelude.Nothing,
      ssoClientId = Prelude.Nothing,
      homeRegion = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      displayName = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      studioUrl = Prelude.Nothing,
      adminRoleArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Configuration of the encryption method that is used for the studio.
studio_studioEncryptionConfiguration :: Lens.Lens' Studio (Prelude.Maybe StudioEncryptionConfiguration)
studio_studioEncryptionConfiguration = Lens.lens (\Studio' {studioEncryptionConfiguration} -> studioEncryptionConfiguration) (\s@Studio' {} a -> s {studioEncryptionConfiguration = a} :: Studio)

-- | The current state of the studio resource.
studio_state :: Lens.Lens' Studio (Prelude.Maybe StudioState)
studio_state = Lens.lens (\Studio' {state} -> state) (\s@Studio' {} a -> s {state = a} :: Studio)

-- | The name of the studio, as included in the URL when accessing it in the
-- Nimble Studio portal.
studio_studioName :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioName = Lens.lens (\Studio' {studioName} -> studioName) (\s@Studio' {} a -> s {studioName = a} :: Studio)

-- | The Amazon Resource Name (ARN) that is assigned to a studio resource and
-- uniquely identifies it. ARNs are unique across all Regions.
studio_arn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_arn = Lens.lens (\Studio' {arn} -> arn) (\s@Studio' {} a -> s {arn = a} :: Studio)

-- | The Unix epoch timestamp in seconds for when the resource was created.
studio_createdAt :: Lens.Lens' Studio (Prelude.Maybe Prelude.UTCTime)
studio_createdAt = Lens.lens (\Studio' {createdAt} -> createdAt) (\s@Studio' {} a -> s {createdAt = a} :: Studio) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for a studio resource. In Nimble Studio, all other
-- resources are contained in a studio resource.
studio_studioId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioId = Lens.lens (\Studio' {studioId} -> studioId) (\s@Studio' {} a -> s {studioId = a} :: Studio)

-- | The IAM role that studio users assume when logging in to the Nimble
-- Studio portal.
studio_userRoleArn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_userRoleArn = Lens.lens (\Studio' {userRoleArn} -> userRoleArn) (\s@Studio' {} a -> s {userRoleArn = a} :: Studio)

-- | The Amazon Web Services SSO application client ID used to integrate with
-- Amazon Web Services SSO to enable Amazon Web Services SSO users to log
-- in to Nimble portal.
studio_ssoClientId :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_ssoClientId = Lens.lens (\Studio' {ssoClientId} -> ssoClientId) (\s@Studio' {} a -> s {ssoClientId = a} :: Studio)

-- | The Amazon Web Services Region where the studio resource is located.
studio_homeRegion :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_homeRegion = Lens.lens (\Studio' {homeRegion} -> homeRegion) (\s@Studio' {} a -> s {homeRegion = a} :: Studio)

-- | Additional detail on the studio state.
studio_statusMessage :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_statusMessage = Lens.lens (\Studio' {statusMessage} -> statusMessage) (\s@Studio' {} a -> s {statusMessage = a} :: Studio)

-- | A friendly name for the studio.
studio_displayName :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_displayName = Lens.lens (\Studio' {displayName} -> displayName) (\s@Studio' {} a -> s {displayName = a} :: Studio)

-- | The Unix epoch timestamp in seconds for when the resource was updated.
studio_updatedAt :: Lens.Lens' Studio (Prelude.Maybe Prelude.UTCTime)
studio_updatedAt = Lens.lens (\Studio' {updatedAt} -> updatedAt) (\s@Studio' {} a -> s {updatedAt = a} :: Studio) Prelude.. Lens.mapping Core._Time

-- | The address of the web page for the studio.
studio_studioUrl :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_studioUrl = Lens.lens (\Studio' {studioUrl} -> studioUrl) (\s@Studio' {} a -> s {studioUrl = a} :: Studio)

-- | The IAM role that studio admins assume when logging in to the Nimble
-- Studio portal.
studio_adminRoleArn :: Lens.Lens' Studio (Prelude.Maybe Prelude.Text)
studio_adminRoleArn = Lens.lens (\Studio' {adminRoleArn} -> adminRoleArn) (\s@Studio' {} a -> s {adminRoleArn = a} :: Studio)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
studio_tags :: Lens.Lens' Studio (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
studio_tags = Lens.lens (\Studio' {tags} -> tags) (\s@Studio' {} a -> s {tags = a} :: Studio) Prelude.. Lens.mapping Lens.coerced

-- | Status codes that provide additional detail on the studio state.
studio_statusCode :: Lens.Lens' Studio (Prelude.Maybe StudioStatusCode)
studio_statusCode = Lens.lens (\Studio' {statusCode} -> statusCode) (\s@Studio' {} a -> s {statusCode = a} :: Studio)

instance Core.FromJSON Studio where
  parseJSON =
    Core.withObject
      "Studio"
      ( \x ->
          Studio'
            Prelude.<$> (x Core..:? "studioEncryptionConfiguration")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "studioName")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "studioId")
            Prelude.<*> (x Core..:? "userRoleArn")
            Prelude.<*> (x Core..:? "ssoClientId")
            Prelude.<*> (x Core..:? "homeRegion")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "studioUrl")
            Prelude.<*> (x Core..:? "adminRoleArn")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "statusCode")
      )

instance Prelude.Hashable Studio

instance Prelude.NFData Studio
