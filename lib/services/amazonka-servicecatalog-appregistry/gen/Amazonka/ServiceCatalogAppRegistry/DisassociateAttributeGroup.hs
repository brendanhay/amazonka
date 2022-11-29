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
-- Module      : Amazonka.ServiceCatalogAppRegistry.DisassociateAttributeGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an attribute group from an application to remove the extra
-- attributes contained in the attribute group from the application\'s
-- metadata. This operation reverts @AssociateAttributeGroup@.
module Amazonka.ServiceCatalogAppRegistry.DisassociateAttributeGroup
  ( -- * Creating a Request
    DisassociateAttributeGroup (..),
    newDisassociateAttributeGroup,

    -- * Request Lenses
    disassociateAttributeGroup_application,
    disassociateAttributeGroup_attributeGroup,

    -- * Destructuring the Response
    DisassociateAttributeGroupResponse (..),
    newDisassociateAttributeGroupResponse,

    -- * Response Lenses
    disassociateAttributeGroupResponse_applicationArn,
    disassociateAttributeGroupResponse_attributeGroupArn,
    disassociateAttributeGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newDisassociateAttributeGroup' smart constructor.
data DisassociateAttributeGroup = DisassociateAttributeGroup'
  { -- | The name or ID of the application.
    application :: Prelude.Text,
    -- | The name or ID of the attribute group that holds the attributes to
    -- describe the application.
    attributeGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAttributeGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'disassociateAttributeGroup_application' - The name or ID of the application.
--
-- 'attributeGroup', 'disassociateAttributeGroup_attributeGroup' - The name or ID of the attribute group that holds the attributes to
-- describe the application.
newDisassociateAttributeGroup ::
  -- | 'application'
  Prelude.Text ->
  -- | 'attributeGroup'
  Prelude.Text ->
  DisassociateAttributeGroup
newDisassociateAttributeGroup
  pApplication_
  pAttributeGroup_ =
    DisassociateAttributeGroup'
      { application =
          pApplication_,
        attributeGroup = pAttributeGroup_
      }

-- | The name or ID of the application.
disassociateAttributeGroup_application :: Lens.Lens' DisassociateAttributeGroup Prelude.Text
disassociateAttributeGroup_application = Lens.lens (\DisassociateAttributeGroup' {application} -> application) (\s@DisassociateAttributeGroup' {} a -> s {application = a} :: DisassociateAttributeGroup)

-- | The name or ID of the attribute group that holds the attributes to
-- describe the application.
disassociateAttributeGroup_attributeGroup :: Lens.Lens' DisassociateAttributeGroup Prelude.Text
disassociateAttributeGroup_attributeGroup = Lens.lens (\DisassociateAttributeGroup' {attributeGroup} -> attributeGroup) (\s@DisassociateAttributeGroup' {} a -> s {attributeGroup = a} :: DisassociateAttributeGroup)

instance Core.AWSRequest DisassociateAttributeGroup where
  type
    AWSResponse DisassociateAttributeGroup =
      DisassociateAttributeGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateAttributeGroupResponse'
            Prelude.<$> (x Core..?> "applicationArn")
            Prelude.<*> (x Core..?> "attributeGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateAttributeGroup where
  hashWithSalt _salt DisassociateAttributeGroup' {..} =
    _salt `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` attributeGroup

instance Prelude.NFData DisassociateAttributeGroup where
  rnf DisassociateAttributeGroup' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf attributeGroup

instance Core.ToHeaders DisassociateAttributeGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DisassociateAttributeGroup where
  toPath DisassociateAttributeGroup' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS application,
        "/attribute-groups/",
        Core.toBS attributeGroup
      ]

instance Core.ToQuery DisassociateAttributeGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateAttributeGroupResponse' smart constructor.
data DisassociateAttributeGroupResponse = DisassociateAttributeGroupResponse'
  { -- | The Amazon resource name (ARN) that specifies the application.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon resource name (ARN) that specifies the attribute group.
    attributeGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAttributeGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'disassociateAttributeGroupResponse_applicationArn' - The Amazon resource name (ARN) that specifies the application.
--
-- 'attributeGroupArn', 'disassociateAttributeGroupResponse_attributeGroupArn' - The Amazon resource name (ARN) that specifies the attribute group.
--
-- 'httpStatus', 'disassociateAttributeGroupResponse_httpStatus' - The response's http status code.
newDisassociateAttributeGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateAttributeGroupResponse
newDisassociateAttributeGroupResponse pHttpStatus_ =
  DisassociateAttributeGroupResponse'
    { applicationArn =
        Prelude.Nothing,
      attributeGroupArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon resource name (ARN) that specifies the application.
disassociateAttributeGroupResponse_applicationArn :: Lens.Lens' DisassociateAttributeGroupResponse (Prelude.Maybe Prelude.Text)
disassociateAttributeGroupResponse_applicationArn = Lens.lens (\DisassociateAttributeGroupResponse' {applicationArn} -> applicationArn) (\s@DisassociateAttributeGroupResponse' {} a -> s {applicationArn = a} :: DisassociateAttributeGroupResponse)

-- | The Amazon resource name (ARN) that specifies the attribute group.
disassociateAttributeGroupResponse_attributeGroupArn :: Lens.Lens' DisassociateAttributeGroupResponse (Prelude.Maybe Prelude.Text)
disassociateAttributeGroupResponse_attributeGroupArn = Lens.lens (\DisassociateAttributeGroupResponse' {attributeGroupArn} -> attributeGroupArn) (\s@DisassociateAttributeGroupResponse' {} a -> s {attributeGroupArn = a} :: DisassociateAttributeGroupResponse)

-- | The response's http status code.
disassociateAttributeGroupResponse_httpStatus :: Lens.Lens' DisassociateAttributeGroupResponse Prelude.Int
disassociateAttributeGroupResponse_httpStatus = Lens.lens (\DisassociateAttributeGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateAttributeGroupResponse' {} a -> s {httpStatus = a} :: DisassociateAttributeGroupResponse)

instance
  Prelude.NFData
    DisassociateAttributeGroupResponse
  where
  rnf DisassociateAttributeGroupResponse' {..} =
    Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf attributeGroupArn
      `Prelude.seq` Prelude.rnf httpStatus
