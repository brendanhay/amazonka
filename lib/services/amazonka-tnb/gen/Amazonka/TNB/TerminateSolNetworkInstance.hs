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
-- Module      : Amazonka.TNB.TerminateSolNetworkInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates a network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- You must terminate a network instance before you can delete it.
module Amazonka.TNB.TerminateSolNetworkInstance
  ( -- * Creating a Request
    TerminateSolNetworkInstance (..),
    newTerminateSolNetworkInstance,

    -- * Request Lenses
    terminateSolNetworkInstance_tags,
    terminateSolNetworkInstance_nsInstanceId,

    -- * Destructuring the Response
    TerminateSolNetworkInstanceResponse (..),
    newTerminateSolNetworkInstanceResponse,

    -- * Response Lenses
    terminateSolNetworkInstanceResponse_nsLcmOpOccId,
    terminateSolNetworkInstanceResponse_tags,
    terminateSolNetworkInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newTerminateSolNetworkInstance' smart constructor.
data TerminateSolNetworkInstance = TerminateSolNetworkInstance'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. When you use this API,
    -- the tags are transferred to the network operation that is created. Use
    -- tags to search and filter your resources or track your Amazon Web
    -- Services costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | ID of the network instance.
    nsInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateSolNetworkInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'terminateSolNetworkInstance_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
--
-- 'nsInstanceId', 'terminateSolNetworkInstance_nsInstanceId' - ID of the network instance.
newTerminateSolNetworkInstance ::
  -- | 'nsInstanceId'
  Prelude.Text ->
  TerminateSolNetworkInstance
newTerminateSolNetworkInstance pNsInstanceId_ =
  TerminateSolNetworkInstance'
    { tags =
        Prelude.Nothing,
      nsInstanceId = pNsInstanceId_
    }

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
terminateSolNetworkInstance_tags :: Lens.Lens' TerminateSolNetworkInstance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
terminateSolNetworkInstance_tags = Lens.lens (\TerminateSolNetworkInstance' {tags} -> tags) (\s@TerminateSolNetworkInstance' {} a -> s {tags = a} :: TerminateSolNetworkInstance) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | ID of the network instance.
terminateSolNetworkInstance_nsInstanceId :: Lens.Lens' TerminateSolNetworkInstance Prelude.Text
terminateSolNetworkInstance_nsInstanceId = Lens.lens (\TerminateSolNetworkInstance' {nsInstanceId} -> nsInstanceId) (\s@TerminateSolNetworkInstance' {} a -> s {nsInstanceId = a} :: TerminateSolNetworkInstance)

instance Core.AWSRequest TerminateSolNetworkInstance where
  type
    AWSResponse TerminateSolNetworkInstance =
      TerminateSolNetworkInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateSolNetworkInstanceResponse'
            Prelude.<$> (x Data..?> "nsLcmOpOccId")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateSolNetworkInstance where
  hashWithSalt _salt TerminateSolNetworkInstance' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` nsInstanceId

instance Prelude.NFData TerminateSolNetworkInstance where
  rnf TerminateSolNetworkInstance' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nsInstanceId

instance Data.ToHeaders TerminateSolNetworkInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateSolNetworkInstance where
  toJSON TerminateSolNetworkInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [("tags" Data..=) Prelude.<$> tags]
      )

instance Data.ToPath TerminateSolNetworkInstance where
  toPath TerminateSolNetworkInstance' {..} =
    Prelude.mconcat
      [ "/sol/nslcm/v1/ns_instances/",
        Data.toBS nsInstanceId,
        "/terminate"
      ]

instance Data.ToQuery TerminateSolNetworkInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateSolNetworkInstanceResponse' smart constructor.
data TerminateSolNetworkInstanceResponse = TerminateSolNetworkInstanceResponse'
  { -- | The identifier of the network operation.
    nsLcmOpOccId :: Prelude.Maybe Prelude.Text,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. When you use this API,
    -- the tags are transferred to the network operation that is created. Use
    -- tags to search and filter your resources or track your Amazon Web
    -- Services costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateSolNetworkInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsLcmOpOccId', 'terminateSolNetworkInstanceResponse_nsLcmOpOccId' - The identifier of the network operation.
--
-- 'tags', 'terminateSolNetworkInstanceResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
--
-- 'httpStatus', 'terminateSolNetworkInstanceResponse_httpStatus' - The response's http status code.
newTerminateSolNetworkInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TerminateSolNetworkInstanceResponse
newTerminateSolNetworkInstanceResponse pHttpStatus_ =
  TerminateSolNetworkInstanceResponse'
    { nsLcmOpOccId =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the network operation.
terminateSolNetworkInstanceResponse_nsLcmOpOccId :: Lens.Lens' TerminateSolNetworkInstanceResponse (Prelude.Maybe Prelude.Text)
terminateSolNetworkInstanceResponse_nsLcmOpOccId = Lens.lens (\TerminateSolNetworkInstanceResponse' {nsLcmOpOccId} -> nsLcmOpOccId) (\s@TerminateSolNetworkInstanceResponse' {} a -> s {nsLcmOpOccId = a} :: TerminateSolNetworkInstanceResponse)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
terminateSolNetworkInstanceResponse_tags :: Lens.Lens' TerminateSolNetworkInstanceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
terminateSolNetworkInstanceResponse_tags = Lens.lens (\TerminateSolNetworkInstanceResponse' {tags} -> tags) (\s@TerminateSolNetworkInstanceResponse' {} a -> s {tags = a} :: TerminateSolNetworkInstanceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
terminateSolNetworkInstanceResponse_httpStatus :: Lens.Lens' TerminateSolNetworkInstanceResponse Prelude.Int
terminateSolNetworkInstanceResponse_httpStatus = Lens.lens (\TerminateSolNetworkInstanceResponse' {httpStatus} -> httpStatus) (\s@TerminateSolNetworkInstanceResponse' {} a -> s {httpStatus = a} :: TerminateSolNetworkInstanceResponse)

instance
  Prelude.NFData
    TerminateSolNetworkInstanceResponse
  where
  rnf TerminateSolNetworkInstanceResponse' {..} =
    Prelude.rnf nsLcmOpOccId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
