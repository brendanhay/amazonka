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
-- Module      : Amazonka.Outposts.CreateSite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a site for an Outpost.
module Amazonka.Outposts.CreateSite
  ( -- * Creating a Request
    CreateSite (..),
    newCreateSite,

    -- * Request Lenses
    createSite_tags,
    createSite_shippingAddress,
    createSite_operatingAddress,
    createSite_description,
    createSite_notes,
    createSite_rackPhysicalProperties,
    createSite_name,

    -- * Destructuring the Response
    CreateSiteResponse (..),
    newCreateSiteResponse,

    -- * Response Lenses
    createSiteResponse_site,
    createSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSite' smart constructor.
data CreateSite = CreateSite'
  { -- | The tags to apply to a site.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The location to ship the hardware. This address might be different from
    -- the operating address.
    shippingAddress :: Prelude.Maybe Address,
    -- | The location to install and power on the hardware. This address might be
    -- different from the shipping address.
    operatingAddress :: Prelude.Maybe Address,
    description :: Prelude.Maybe Prelude.Text,
    -- | Additional information that you provide about site access requirements,
    -- electrician scheduling, personal protective equipment, or regulation of
    -- equipment materials that could affect your installation process.
    notes :: Prelude.Maybe Prelude.Text,
    -- | Information about the physical and logistical details for the rack at
    -- this site. For more information about hardware requirements for racks,
    -- see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#checklist Network readiness checklist>
    -- in the Amazon Web Services Outposts User Guide.
    rackPhysicalProperties :: Prelude.Maybe RackPhysicalProperties,
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSite_tags' - The tags to apply to a site.
--
-- 'shippingAddress', 'createSite_shippingAddress' - The location to ship the hardware. This address might be different from
-- the operating address.
--
-- 'operatingAddress', 'createSite_operatingAddress' - The location to install and power on the hardware. This address might be
-- different from the shipping address.
--
-- 'description', 'createSite_description' - Undocumented member.
--
-- 'notes', 'createSite_notes' - Additional information that you provide about site access requirements,
-- electrician scheduling, personal protective equipment, or regulation of
-- equipment materials that could affect your installation process.
--
-- 'rackPhysicalProperties', 'createSite_rackPhysicalProperties' - Information about the physical and logistical details for the rack at
-- this site. For more information about hardware requirements for racks,
-- see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#checklist Network readiness checklist>
-- in the Amazon Web Services Outposts User Guide.
--
-- 'name', 'createSite_name' - Undocumented member.
newCreateSite ::
  -- | 'name'
  Prelude.Text ->
  CreateSite
newCreateSite pName_ =
  CreateSite'
    { tags = Prelude.Nothing,
      shippingAddress = Prelude.Nothing,
      operatingAddress = Prelude.Nothing,
      description = Prelude.Nothing,
      notes = Prelude.Nothing,
      rackPhysicalProperties = Prelude.Nothing,
      name = pName_
    }

-- | The tags to apply to a site.
createSite_tags :: Lens.Lens' CreateSite (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSite_tags = Lens.lens (\CreateSite' {tags} -> tags) (\s@CreateSite' {} a -> s {tags = a} :: CreateSite) Prelude.. Lens.mapping Lens.coerced

-- | The location to ship the hardware. This address might be different from
-- the operating address.
createSite_shippingAddress :: Lens.Lens' CreateSite (Prelude.Maybe Address)
createSite_shippingAddress = Lens.lens (\CreateSite' {shippingAddress} -> shippingAddress) (\s@CreateSite' {} a -> s {shippingAddress = a} :: CreateSite)

-- | The location to install and power on the hardware. This address might be
-- different from the shipping address.
createSite_operatingAddress :: Lens.Lens' CreateSite (Prelude.Maybe Address)
createSite_operatingAddress = Lens.lens (\CreateSite' {operatingAddress} -> operatingAddress) (\s@CreateSite' {} a -> s {operatingAddress = a} :: CreateSite)

-- | Undocumented member.
createSite_description :: Lens.Lens' CreateSite (Prelude.Maybe Prelude.Text)
createSite_description = Lens.lens (\CreateSite' {description} -> description) (\s@CreateSite' {} a -> s {description = a} :: CreateSite)

-- | Additional information that you provide about site access requirements,
-- electrician scheduling, personal protective equipment, or regulation of
-- equipment materials that could affect your installation process.
createSite_notes :: Lens.Lens' CreateSite (Prelude.Maybe Prelude.Text)
createSite_notes = Lens.lens (\CreateSite' {notes} -> notes) (\s@CreateSite' {} a -> s {notes = a} :: CreateSite)

-- | Information about the physical and logistical details for the rack at
-- this site. For more information about hardware requirements for racks,
-- see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-requirements.html#checklist Network readiness checklist>
-- in the Amazon Web Services Outposts User Guide.
createSite_rackPhysicalProperties :: Lens.Lens' CreateSite (Prelude.Maybe RackPhysicalProperties)
createSite_rackPhysicalProperties = Lens.lens (\CreateSite' {rackPhysicalProperties} -> rackPhysicalProperties) (\s@CreateSite' {} a -> s {rackPhysicalProperties = a} :: CreateSite)

-- | Undocumented member.
createSite_name :: Lens.Lens' CreateSite Prelude.Text
createSite_name = Lens.lens (\CreateSite' {name} -> name) (\s@CreateSite' {} a -> s {name = a} :: CreateSite)

instance Core.AWSRequest CreateSite where
  type AWSResponse CreateSite = CreateSiteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSiteResponse'
            Prelude.<$> (x Core..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSite where
  hashWithSalt _salt CreateSite' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` shippingAddress
      `Prelude.hashWithSalt` operatingAddress
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` rackPhysicalProperties
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSite where
  rnf CreateSite' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf shippingAddress
      `Prelude.seq` Prelude.rnf operatingAddress
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf rackPhysicalProperties
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSite where
  toJSON CreateSite' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ShippingAddress" Core..=)
              Prelude.<$> shippingAddress,
            ("OperatingAddress" Core..=)
              Prelude.<$> operatingAddress,
            ("Description" Core..=) Prelude.<$> description,
            ("Notes" Core..=) Prelude.<$> notes,
            ("RackPhysicalProperties" Core..=)
              Prelude.<$> rackPhysicalProperties,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateSite where
  toPath = Prelude.const "/sites"

instance Core.ToQuery CreateSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSiteResponse' smart constructor.
data CreateSiteResponse = CreateSiteResponse'
  { site :: Prelude.Maybe Site,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'site', 'createSiteResponse_site' - Undocumented member.
--
-- 'httpStatus', 'createSiteResponse_httpStatus' - The response's http status code.
newCreateSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSiteResponse
newCreateSiteResponse pHttpStatus_ =
  CreateSiteResponse'
    { site = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createSiteResponse_site :: Lens.Lens' CreateSiteResponse (Prelude.Maybe Site)
createSiteResponse_site = Lens.lens (\CreateSiteResponse' {site} -> site) (\s@CreateSiteResponse' {} a -> s {site = a} :: CreateSiteResponse)

-- | The response's http status code.
createSiteResponse_httpStatus :: Lens.Lens' CreateSiteResponse Prelude.Int
createSiteResponse_httpStatus = Lens.lens (\CreateSiteResponse' {httpStatus} -> httpStatus) (\s@CreateSiteResponse' {} a -> s {httpStatus = a} :: CreateSiteResponse)

instance Prelude.NFData CreateSiteResponse where
  rnf CreateSiteResponse' {..} =
    Prelude.rnf site
      `Prelude.seq` Prelude.rnf httpStatus
