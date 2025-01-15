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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network site.
module Amazonka.PrivateNetworks.CreateNetworkSite
  ( -- * Creating a Request
    CreateNetworkSite (..),
    newCreateNetworkSite,

    -- * Request Lenses
    createNetworkSite_availabilityZone,
    createNetworkSite_availabilityZoneId,
    createNetworkSite_clientToken,
    createNetworkSite_description,
    createNetworkSite_pendingPlan,
    createNetworkSite_tags,
    createNetworkSite_networkArn,
    createNetworkSite_networkSiteName,

    -- * Destructuring the Response
    CreateNetworkSiteResponse (..),
    newCreateNetworkSiteResponse,

    -- * Response Lenses
    createNetworkSiteResponse_networkSite,
    createNetworkSiteResponse_tags,
    createNetworkSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetworkSite' smart constructor.
data CreateNetworkSite = CreateNetworkSite'
  { -- | The Availability Zone that is the parent of this site. You can\'t change
    -- the Availability Zone after you create the site.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Availability Zone that is the parent of this site. You
    -- can\'t change the Availability Zone after you create the site.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the site.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the pending plan for this site.
    pendingPlan :: Prelude.Maybe SitePlan,
    -- | The tags to apply to the network site.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
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
-- 'availabilityZone', 'createNetworkSite_availabilityZone' - The Availability Zone that is the parent of this site. You can\'t change
-- the Availability Zone after you create the site.
--
-- 'availabilityZoneId', 'createNetworkSite_availabilityZoneId' - The ID of the Availability Zone that is the parent of this site. You
-- can\'t change the Availability Zone after you create the site.
--
-- 'clientToken', 'createNetworkSite_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'description', 'createNetworkSite_description' - The description of the site.
--
-- 'pendingPlan', 'createNetworkSite_pendingPlan' - Information about the pending plan for this site.
--
-- 'tags', 'createNetworkSite_tags' - The tags to apply to the network site.
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
    { availabilityZone =
        Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      pendingPlan = Prelude.Nothing,
      tags = Prelude.Nothing,
      networkArn = pNetworkArn_,
      networkSiteName = pNetworkSiteName_
    }

-- | The Availability Zone that is the parent of this site. You can\'t change
-- the Availability Zone after you create the site.
createNetworkSite_availabilityZone :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_availabilityZone = Lens.lens (\CreateNetworkSite' {availabilityZone} -> availabilityZone) (\s@CreateNetworkSite' {} a -> s {availabilityZone = a} :: CreateNetworkSite)

-- | The ID of the Availability Zone that is the parent of this site. You
-- can\'t change the Availability Zone after you create the site.
createNetworkSite_availabilityZoneId :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_availabilityZoneId = Lens.lens (\CreateNetworkSite' {availabilityZoneId} -> availabilityZoneId) (\s@CreateNetworkSite' {} a -> s {availabilityZoneId = a} :: CreateNetworkSite)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
createNetworkSite_clientToken :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_clientToken = Lens.lens (\CreateNetworkSite' {clientToken} -> clientToken) (\s@CreateNetworkSite' {} a -> s {clientToken = a} :: CreateNetworkSite)

-- | The description of the site.
createNetworkSite_description :: Lens.Lens' CreateNetworkSite (Prelude.Maybe Prelude.Text)
createNetworkSite_description = Lens.lens (\CreateNetworkSite' {description} -> description) (\s@CreateNetworkSite' {} a -> s {description = a} :: CreateNetworkSite)

-- | Information about the pending plan for this site.
createNetworkSite_pendingPlan :: Lens.Lens' CreateNetworkSite (Prelude.Maybe SitePlan)
createNetworkSite_pendingPlan = Lens.lens (\CreateNetworkSite' {pendingPlan} -> pendingPlan) (\s@CreateNetworkSite' {} a -> s {pendingPlan = a} :: CreateNetworkSite)

-- | The tags to apply to the network site.
createNetworkSite_tags :: Lens.Lens' CreateNetworkSite (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNetworkSite_tags = Lens.lens (\CreateNetworkSite' {tags} -> tags) (\s@CreateNetworkSite' {} a -> s {tags = a} :: CreateNetworkSite) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

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
            Prelude.<$> (x Data..?> "networkSite")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkSite where
  hashWithSalt _salt CreateNetworkSite' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pendingPlan
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` networkArn
      `Prelude.hashWithSalt` networkSiteName

instance Prelude.NFData CreateNetworkSite where
  rnf CreateNetworkSite' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf availabilityZoneId `Prelude.seq`
        Prelude.rnf clientToken `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf pendingPlan `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf networkArn `Prelude.seq`
                  Prelude.rnf networkSiteName

instance Data.ToHeaders CreateNetworkSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNetworkSite where
  toJSON CreateNetworkSite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("availabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("availabilityZoneId" Data..=)
              Prelude.<$> availabilityZoneId,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("pendingPlan" Data..=) Prelude.<$> pendingPlan,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("networkArn" Data..= networkArn),
            Prelude.Just
              ("networkSiteName" Data..= networkSiteName)
          ]
      )

instance Data.ToPath CreateNetworkSite where
  toPath = Prelude.const "/v1/network-sites"

instance Data.ToQuery CreateNetworkSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNetworkSiteResponse' smart constructor.
data CreateNetworkSiteResponse = CreateNetworkSiteResponse'
  { -- | Information about the network site.
    networkSite :: Prelude.Maybe NetworkSite,
    -- | The network site tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
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
-- 'networkSite', 'createNetworkSiteResponse_networkSite' - Information about the network site.
--
-- 'tags', 'createNetworkSiteResponse_tags' - The network site tags.
--
-- 'httpStatus', 'createNetworkSiteResponse_httpStatus' - The response's http status code.
newCreateNetworkSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNetworkSiteResponse
newCreateNetworkSiteResponse pHttpStatus_ =
  CreateNetworkSiteResponse'
    { networkSite =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the network site.
createNetworkSiteResponse_networkSite :: Lens.Lens' CreateNetworkSiteResponse (Prelude.Maybe NetworkSite)
createNetworkSiteResponse_networkSite = Lens.lens (\CreateNetworkSiteResponse' {networkSite} -> networkSite) (\s@CreateNetworkSiteResponse' {} a -> s {networkSite = a} :: CreateNetworkSiteResponse)

-- | The network site tags.
createNetworkSiteResponse_tags :: Lens.Lens' CreateNetworkSiteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNetworkSiteResponse_tags = Lens.lens (\CreateNetworkSiteResponse' {tags} -> tags) (\s@CreateNetworkSiteResponse' {} a -> s {tags = a} :: CreateNetworkSiteResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
createNetworkSiteResponse_httpStatus :: Lens.Lens' CreateNetworkSiteResponse Prelude.Int
createNetworkSiteResponse_httpStatus = Lens.lens (\CreateNetworkSiteResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkSiteResponse' {} a -> s {httpStatus = a} :: CreateNetworkSiteResponse)

instance Prelude.NFData CreateNetworkSiteResponse where
  rnf CreateNetworkSiteResponse' {..} =
    Prelude.rnf networkSite `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf httpStatus
