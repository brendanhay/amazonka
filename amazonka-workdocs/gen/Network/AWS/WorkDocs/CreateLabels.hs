{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkDocs.CreateLabels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified list of labels to the given resource (a document or
-- folder)
module Network.AWS.WorkDocs.CreateLabels
  ( -- * Creating a Request
    CreateLabels (..),
    newCreateLabels,

    -- * Request Lenses
    createLabels_authenticationToken,
    createLabels_resourceId,
    createLabels_labels,

    -- * Destructuring the Response
    CreateLabelsResponse (..),
    newCreateLabelsResponse,

    -- * Response Lenses
    createLabelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newCreateLabels' smart constructor.
data CreateLabels = CreateLabels'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the resource.
    resourceId :: Prelude.Text,
    -- | List of labels to add to the resource.
    labels :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLabels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'createLabels_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'resourceId', 'createLabels_resourceId' - The ID of the resource.
--
-- 'labels', 'createLabels_labels' - List of labels to add to the resource.
newCreateLabels ::
  -- | 'resourceId'
  Prelude.Text ->
  CreateLabels
newCreateLabels pResourceId_ =
  CreateLabels'
    { authenticationToken =
        Prelude.Nothing,
      resourceId = pResourceId_,
      labels = Prelude.mempty
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createLabels_authenticationToken :: Lens.Lens' CreateLabels (Prelude.Maybe Prelude.Text)
createLabels_authenticationToken = Lens.lens (\CreateLabels' {authenticationToken} -> authenticationToken) (\s@CreateLabels' {} a -> s {authenticationToken = a} :: CreateLabels) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the resource.
createLabels_resourceId :: Lens.Lens' CreateLabels Prelude.Text
createLabels_resourceId = Lens.lens (\CreateLabels' {resourceId} -> resourceId) (\s@CreateLabels' {} a -> s {resourceId = a} :: CreateLabels)

-- | List of labels to add to the resource.
createLabels_labels :: Lens.Lens' CreateLabels [Prelude.Text]
createLabels_labels = Lens.lens (\CreateLabels' {labels} -> labels) (\s@CreateLabels' {} a -> s {labels = a} :: CreateLabels) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreateLabels where
  type Rs CreateLabels = CreateLabelsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateLabelsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLabels

instance Prelude.NFData CreateLabels

instance Prelude.ToHeaders CreateLabels where
  toHeaders CreateLabels' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON CreateLabels where
  toJSON CreateLabels' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Labels" Prelude..= labels)]
      )

instance Prelude.ToPath CreateLabels where
  toPath CreateLabels' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Prelude.toBS resourceId,
        "/labels"
      ]

instance Prelude.ToQuery CreateLabels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLabelsResponse' smart constructor.
data CreateLabelsResponse = CreateLabelsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLabelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLabelsResponse_httpStatus' - The response's http status code.
newCreateLabelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLabelsResponse
newCreateLabelsResponse pHttpStatus_ =
  CreateLabelsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createLabelsResponse_httpStatus :: Lens.Lens' CreateLabelsResponse Prelude.Int
createLabelsResponse_httpStatus = Lens.lens (\CreateLabelsResponse' {httpStatus} -> httpStatus) (\s@CreateLabelsResponse' {} a -> s {httpStatus = a} :: CreateLabelsResponse)

instance Prelude.NFData CreateLabelsResponse
