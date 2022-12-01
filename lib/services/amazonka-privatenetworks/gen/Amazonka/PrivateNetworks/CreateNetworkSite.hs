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
-- Module      : Amazonka.PrivateNetworks.CreateNetworkSite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network site.
module Amazonka.PrivateNetworks.CreateNetworkSite
  ( -- * Creating a Request
    CreateNetworkSite (..),
    newCreateNetworkSite,

    -- * Request Lenses
    createNetworkSite_tags,
    createNetworkSite_clientToken,
    createNetworkSite_description,
    createNetworkSite_availabilityZone,
    createNetworkSite_pendingPlan,
    createNetworkSite_availabilityZoneId,
    createNetworkSite_networkArn,
    createNetworkSite_networkSiteName,

    -- * Destructuring the Response
    CreateNetworkSiteResponse (..),
    newCreateNetworkSiteResponse,

    -- * Response Lenses
    createNetworkSiteResponse_tags,
    createNetworkSiteResponse_networkSite,
    createNetworkSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetworkSite' smart constructor.
data CreateNetworkSite = CreateNetworkSite'
  { -- | The tags to apply to the network site.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the site.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone that is the parent of this site. You can\'t change
    -- the Availability Zone after you create the site.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Information about the pending plan for this site.
    pendingPlan :: Prelude.Maybe SitePlan,
    -- | The ID of the Availability Zone that is the parent of this site. You
    -- can\'t change the Availability Zone after you create the site.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network.
    networkArn :: Prelude.Text,
    -- | The name of the site. You can\'t change the name after you create the
    -- site.
    networkSiteName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createNetworkSite_tags' - The tags to apply to the network site.
--
-- 'clientToken', 'createNetworkSite_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'description', 'createNetworkSite_description' - The description of the site.
--
-- 'availabilityZone', 'createNetworkSite_availabilityZone' - The Availability Zone that is the parent of this site. You can\'t change
-- the Availability Zone after you create the site.
--
-- 'pendingPlan', 'createNetworkSite_pendingPlan' - Information about the pending plan for this site.
--
-- 'availabilityZoneId', 'createNetworkSite_availabilityZoneId' - The ID of the Availability Zone that is the parent of this site. You
-- can\'t change the Availability Zone after you create the site.
--
-- 'networkArn', 'createNetworkSite_networkArn' - The Amazon Resource Name (ARN) of the network.
--
-- 'networkSiteName', 'createNetworkSite_networkSiteName' - The name of the site. You can\'t change the name after you create the
-- site.
newCreateNetworkSite ::
  -- | 'networkArn'
  Prelude.Text ->
  -- | 'networkSiteName'
  Prelude.Text ->
  CreateNetworkSite
newCreateNetworkSite pNetworkArn_ pNetworkSiteName_ =
  CreateNetworkSite'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      pendingPlan = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      networkArn = pNetworkArn_,
      networkSiteName = pNetworkSiteName_
    }

-- | The tags to apply to the network site.
createNetworkSite_tags :: Lens.Lens' CreateNetworkSite (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNetworkSite_tags = Lens.lens (\CreateNetworkSite' {tags} -> tags) (\s@CreateNetworkSite' {} a -> s {tags = a} :: CreateNetworkSite) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
createNetworkSite_clientToken :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_clientToken = Lens.lens (\CreateNetworkSite' {clientToken} -> clientToken) (\s@CreateNetworkSite' {} a -> s {clientToken = a} :: CreateNetworkSite)

-- | The description of the site.
createNetworkSite_description :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_description = Lens.lens (\CreateNetworkSite' {description} -> description) (\s@CreateNetworkSite' {} a -> s {description = a} :: CreateNetworkSite)

-- | The Availability Zone that is the parent of this site. You can\'t change
-- the Availability Zone after you create the site.
createNetworkSite_availabilityZone :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_availabilityZone = Lens.lens (\CreateNetworkSite' {availabilityZone} -> availabilityZone) (\s@CreateNetworkSite' {} a -> s {availabilityZone = a} :: CreateNetworkSite)

-- | Information about the pending plan for this site.
createNetworkSite_pendingPlan :: Lens.Lens' CreateNetworkSite (Prelude.Maybe SitePlan)
createNetworkSite_pendingPlan = Lens.lens (\CreateNetworkSite' {pendingPlan} -> pendingPlan) (\s@CreateNetworkSite' {} a -> s {pendingPlan = a} :: CreateNetworkSite)

-- | The ID of the Availability Zone that is the parent of this site. You
-- can\'t change the Availability Zone after you create the site.
createNetworkSite_availabilityZoneId :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_availabilityZoneId = Lens.lens (\CreateNetworkSite' {availabilityZoneId} -> availabilityZoneId) (\s@CreateNetworkSite' {} a -> s {availabilityZoneId = a} :: CreateNetworkSite)

-- | The Amazon Resource Name (ARN) of the network.
createNetworkSite_networkArn :: Lens.Lens' CreateNetworkSite Prelude.Text
createNetworkSite_networkArn = Lens.lens (\CreateNetworkSite' {networkArn} -> networkArn) (\s@CreateNetworkSite' {} a -> s {networkArn = a} :: CreateNetworkSite)

-- | The name of the site. You can\'t change the name after you create the
-- site.
createNetworkSite_networkSiteName :: Lens.Lens' CreateNetworkSite Prelude.Text
createNetworkSite_networkSiteName = Lens.lens (\CreateNetworkSite' {networkSiteName} -> networkSiteName) (\s@CreateNetworkSite' {} a -> s {networkSiteName = a} :: CreateNetworkSite)

instance Core.AWSRequest CreateNetworkSite where
  type
    AWSResponse CreateNetworkSite =
      CreateNetworkSiteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkSiteResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "networkSite")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkSite where
  hashWithSalt _salt CreateNetworkSite' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` pendingPlan
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` networkArn
      `Prelude.hashWithSalt` networkSiteName

instance Prelude.NFData CreateNetworkSite where
  rnf CreateNetworkSite' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf pendingPlan
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf networkArn
      `Prelude.seq` Prelude.rnf networkSiteName

instance Core.ToHeaders CreateNetworkSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNetworkSite where
  toJSON CreateNetworkSite' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("description" Core..=) Prelude.<$> description,
            ("availabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("pendingPlan" Core..=) Prelude.<$> pendingPlan,
            ("availabilityZoneId" Core..=)
              Prelude.<$> availabilityZoneId,
            Prelude.Just ("networkArn" Core..= networkArn),
            Prelude.Just
              ("networkSiteName" Core..= networkSiteName)
          ]
      )

instance Core.ToPath CreateNetworkSite where
  toPath = Prelude.const "/v1/network-sites"

instance Core.ToQuery CreateNetworkSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNetworkSiteResponse' smart constructor.
data CreateNetworkSiteResponse = CreateNetworkSiteResponse'
  { -- | The network site tags.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Information about the network site.
    networkSite :: Prelude.Maybe NetworkSite,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createNetworkSiteResponse_tags' - The network site tags.
--
-- 'networkSite', 'createNetworkSiteResponse_networkSite' - Information about the network site.
--
-- 'httpStatus', 'createNetworkSiteResponse_httpStatus' - The response's http status code.
newCreateNetworkSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNetworkSiteResponse
newCreateNetworkSiteResponse pHttpStatus_ =
  CreateNetworkSiteResponse'
    { tags = Prelude.Nothing,
      networkSite = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network site tags.
createNetworkSiteResponse_tags :: Lens.Lens' CreateNetworkSiteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNetworkSiteResponse_tags = Lens.lens (\CreateNetworkSiteResponse' {tags} -> tags) (\s@CreateNetworkSiteResponse' {} a -> s {tags = a} :: CreateNetworkSiteResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Information about the network site.
createNetworkSiteResponse_networkSite :: Lens.Lens' CreateNetworkSiteResponse (Prelude.Maybe NetworkSite)
createNetworkSiteResponse_networkSite = Lens.lens (\CreateNetworkSiteResponse' {networkSite} -> networkSite) (\s@CreateNetworkSiteResponse' {} a -> s {networkSite = a} :: CreateNetworkSiteResponse)

-- | The response's http status code.
createNetworkSiteResponse_httpStatus :: Lens.Lens' CreateNetworkSiteResponse Prelude.Int
createNetworkSiteResponse_httpStatus = Lens.lens (\CreateNetworkSiteResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkSiteResponse' {} a -> s {httpStatus = a} :: CreateNetworkSiteResponse)

instance Prelude.NFData CreateNetworkSiteResponse where
  rnf CreateNetworkSiteResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf networkSite
      `Prelude.seq` Prelude.rnf httpStatus
