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
-- Module      : Amazonka.DrS.CreateSourceNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new Source Network resource for a provided VPC ID.
module Amazonka.DrS.CreateSourceNetwork
  ( -- * Creating a Request
    CreateSourceNetwork (..),
    newCreateSourceNetwork,

    -- * Request Lenses
    createSourceNetwork_tags,
    createSourceNetwork_originAccountID,
    createSourceNetwork_originRegion,
    createSourceNetwork_vpcID,

    -- * Destructuring the Response
    CreateSourceNetworkResponse (..),
    newCreateSourceNetworkResponse,

    -- * Response Lenses
    createSourceNetworkResponse_sourceNetworkID,
    createSourceNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSourceNetwork' smart constructor.
data CreateSourceNetwork = CreateSourceNetwork'
  { -- | A set of tags to be associated with the Source Network resource.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Account containing the VPC to protect.
    originAccountID :: Prelude.Text,
    -- | Region containing the VPC to protect.
    originRegion :: Prelude.Text,
    -- | Which VPC ID to protect.
    vpcID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSourceNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSourceNetwork_tags' - A set of tags to be associated with the Source Network resource.
--
-- 'originAccountID', 'createSourceNetwork_originAccountID' - Account containing the VPC to protect.
--
-- 'originRegion', 'createSourceNetwork_originRegion' - Region containing the VPC to protect.
--
-- 'vpcID', 'createSourceNetwork_vpcID' - Which VPC ID to protect.
newCreateSourceNetwork ::
  -- | 'originAccountID'
  Prelude.Text ->
  -- | 'originRegion'
  Prelude.Text ->
  -- | 'vpcID'
  Prelude.Text ->
  CreateSourceNetwork
newCreateSourceNetwork
  pOriginAccountID_
  pOriginRegion_
  pVpcID_ =
    CreateSourceNetwork'
      { tags = Prelude.Nothing,
        originAccountID = pOriginAccountID_,
        originRegion = pOriginRegion_,
        vpcID = pVpcID_
      }

-- | A set of tags to be associated with the Source Network resource.
createSourceNetwork_tags :: Lens.Lens' CreateSourceNetwork (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSourceNetwork_tags = Lens.lens (\CreateSourceNetwork' {tags} -> tags) (\s@CreateSourceNetwork' {} a -> s {tags = a} :: CreateSourceNetwork) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Account containing the VPC to protect.
createSourceNetwork_originAccountID :: Lens.Lens' CreateSourceNetwork Prelude.Text
createSourceNetwork_originAccountID = Lens.lens (\CreateSourceNetwork' {originAccountID} -> originAccountID) (\s@CreateSourceNetwork' {} a -> s {originAccountID = a} :: CreateSourceNetwork)

-- | Region containing the VPC to protect.
createSourceNetwork_originRegion :: Lens.Lens' CreateSourceNetwork Prelude.Text
createSourceNetwork_originRegion = Lens.lens (\CreateSourceNetwork' {originRegion} -> originRegion) (\s@CreateSourceNetwork' {} a -> s {originRegion = a} :: CreateSourceNetwork)

-- | Which VPC ID to protect.
createSourceNetwork_vpcID :: Lens.Lens' CreateSourceNetwork Prelude.Text
createSourceNetwork_vpcID = Lens.lens (\CreateSourceNetwork' {vpcID} -> vpcID) (\s@CreateSourceNetwork' {} a -> s {vpcID = a} :: CreateSourceNetwork)

instance Core.AWSRequest CreateSourceNetwork where
  type
    AWSResponse CreateSourceNetwork =
      CreateSourceNetworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSourceNetworkResponse'
            Prelude.<$> (x Data..?> "sourceNetworkID")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSourceNetwork where
  hashWithSalt _salt CreateSourceNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` originAccountID
      `Prelude.hashWithSalt` originRegion
      `Prelude.hashWithSalt` vpcID

instance Prelude.NFData CreateSourceNetwork where
  rnf CreateSourceNetwork' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf originAccountID
      `Prelude.seq` Prelude.rnf originRegion
      `Prelude.seq` Prelude.rnf vpcID

instance Data.ToHeaders CreateSourceNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSourceNetwork where
  toJSON CreateSourceNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("originAccountID" Data..= originAccountID),
            Prelude.Just ("originRegion" Data..= originRegion),
            Prelude.Just ("vpcID" Data..= vpcID)
          ]
      )

instance Data.ToPath CreateSourceNetwork where
  toPath = Prelude.const "/CreateSourceNetwork"

instance Data.ToQuery CreateSourceNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSourceNetworkResponse' smart constructor.
data CreateSourceNetworkResponse = CreateSourceNetworkResponse'
  { -- | ID of the created Source Network.
    sourceNetworkID :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSourceNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkID', 'createSourceNetworkResponse_sourceNetworkID' - ID of the created Source Network.
--
-- 'httpStatus', 'createSourceNetworkResponse_httpStatus' - The response's http status code.
newCreateSourceNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSourceNetworkResponse
newCreateSourceNetworkResponse pHttpStatus_ =
  CreateSourceNetworkResponse'
    { sourceNetworkID =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ID of the created Source Network.
createSourceNetworkResponse_sourceNetworkID :: Lens.Lens' CreateSourceNetworkResponse (Prelude.Maybe Prelude.Text)
createSourceNetworkResponse_sourceNetworkID = Lens.lens (\CreateSourceNetworkResponse' {sourceNetworkID} -> sourceNetworkID) (\s@CreateSourceNetworkResponse' {} a -> s {sourceNetworkID = a} :: CreateSourceNetworkResponse)

-- | The response's http status code.
createSourceNetworkResponse_httpStatus :: Lens.Lens' CreateSourceNetworkResponse Prelude.Int
createSourceNetworkResponse_httpStatus = Lens.lens (\CreateSourceNetworkResponse' {httpStatus} -> httpStatus) (\s@CreateSourceNetworkResponse' {} a -> s {httpStatus = a} :: CreateSourceNetworkResponse)

instance Prelude.NFData CreateSourceNetworkResponse where
  rnf CreateSourceNetworkResponse' {..} =
    Prelude.rnf sourceNetworkID
      `Prelude.seq` Prelude.rnf httpStatus
