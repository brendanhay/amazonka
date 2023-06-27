{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RolesAnywhere.CreateTrustAnchor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a trust anchor to establish trust between IAM Roles Anywhere and
-- your certificate authority (CA). You can define a trust anchor as a
-- reference to an Private Certificate Authority (Private CA) or by
-- uploading a CA certificate. Your Amazon Web Services workloads can
-- authenticate with the trust anchor using certificates issued by the CA
-- in exchange for temporary Amazon Web Services credentials.
--
-- __Required permissions:__ @rolesanywhere:CreateTrustAnchor@.
module Amazonka.RolesAnywhere.CreateTrustAnchor
  ( -- * Creating a Request
    CreateTrustAnchor (..),
    newCreateTrustAnchor,

    -- * Request Lenses
    createTrustAnchor_enabled,
    createTrustAnchor_notificationSettings,
    createTrustAnchor_tags,
    createTrustAnchor_name,
    createTrustAnchor_source,

    -- * Destructuring the Response
    TrustAnchorDetailResponse (..),
    newTrustAnchorDetailResponse,

    -- * Response Lenses
    trustAnchorDetailResponse_trustAnchor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newCreateTrustAnchor' smart constructor.
data CreateTrustAnchor = CreateTrustAnchor'
  { -- | Specifies whether the trust anchor is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of notification settings to be associated to the trust anchor.
    notificationSettings :: Prelude.Maybe [NotificationSetting],
    -- | The tags to attach to the trust anchor.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the trust anchor.
    name :: Prelude.Text,
    -- | The trust anchor type and its related certificate data.
    source :: Source
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrustAnchor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createTrustAnchor_enabled' - Specifies whether the trust anchor is enabled.
--
-- 'notificationSettings', 'createTrustAnchor_notificationSettings' - A list of notification settings to be associated to the trust anchor.
--
-- 'tags', 'createTrustAnchor_tags' - The tags to attach to the trust anchor.
--
-- 'name', 'createTrustAnchor_name' - The name of the trust anchor.
--
-- 'source', 'createTrustAnchor_source' - The trust anchor type and its related certificate data.
newCreateTrustAnchor ::
  -- | 'name'
  Prelude.Text ->
  -- | 'source'
  Source ->
  CreateTrustAnchor
newCreateTrustAnchor pName_ pSource_ =
  CreateTrustAnchor'
    { enabled = Prelude.Nothing,
      notificationSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      source = pSource_
    }

-- | Specifies whether the trust anchor is enabled.
createTrustAnchor_enabled :: Lens.Lens' CreateTrustAnchor (Prelude.Maybe Prelude.Bool)
createTrustAnchor_enabled = Lens.lens (\CreateTrustAnchor' {enabled} -> enabled) (\s@CreateTrustAnchor' {} a -> s {enabled = a} :: CreateTrustAnchor)

-- | A list of notification settings to be associated to the trust anchor.
createTrustAnchor_notificationSettings :: Lens.Lens' CreateTrustAnchor (Prelude.Maybe [NotificationSetting])
createTrustAnchor_notificationSettings = Lens.lens (\CreateTrustAnchor' {notificationSettings} -> notificationSettings) (\s@CreateTrustAnchor' {} a -> s {notificationSettings = a} :: CreateTrustAnchor) Prelude.. Lens.mapping Lens.coerced

-- | The tags to attach to the trust anchor.
createTrustAnchor_tags :: Lens.Lens' CreateTrustAnchor (Prelude.Maybe [Tag])
createTrustAnchor_tags = Lens.lens (\CreateTrustAnchor' {tags} -> tags) (\s@CreateTrustAnchor' {} a -> s {tags = a} :: CreateTrustAnchor) Prelude.. Lens.mapping Lens.coerced

-- | The name of the trust anchor.
createTrustAnchor_name :: Lens.Lens' CreateTrustAnchor Prelude.Text
createTrustAnchor_name = Lens.lens (\CreateTrustAnchor' {name} -> name) (\s@CreateTrustAnchor' {} a -> s {name = a} :: CreateTrustAnchor)

-- | The trust anchor type and its related certificate data.
createTrustAnchor_source :: Lens.Lens' CreateTrustAnchor Source
createTrustAnchor_source = Lens.lens (\CreateTrustAnchor' {source} -> source) (\s@CreateTrustAnchor' {} a -> s {source = a} :: CreateTrustAnchor)

instance Core.AWSRequest CreateTrustAnchor where
  type
    AWSResponse CreateTrustAnchor =
      TrustAnchorDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateTrustAnchor where
  hashWithSalt _salt CreateTrustAnchor' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` notificationSettings
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source

instance Prelude.NFData CreateTrustAnchor where
  rnf CreateTrustAnchor' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf notificationSettings
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source

instance Data.ToHeaders CreateTrustAnchor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTrustAnchor where
  toJSON CreateTrustAnchor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("notificationSettings" Data..=)
              Prelude.<$> notificationSettings,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("source" Data..= source)
          ]
      )

instance Data.ToPath CreateTrustAnchor where
  toPath = Prelude.const "/trustanchors"

instance Data.ToQuery CreateTrustAnchor where
  toQuery = Prelude.const Prelude.mempty
