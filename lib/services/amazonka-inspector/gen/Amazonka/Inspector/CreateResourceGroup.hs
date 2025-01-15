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
-- Module      : Amazonka.Inspector.CreateResourceGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group using the specified set of tags (key and value
-- pairs) that are used to select the EC2 instances to be included in an
-- Amazon Inspector assessment target. The created resource group is then
-- used to create an Amazon Inspector assessment target. For more
-- information, see CreateAssessmentTarget.
module Amazonka.Inspector.CreateResourceGroup
  ( -- * Creating a Request
    CreateResourceGroup (..),
    newCreateResourceGroup,

    -- * Request Lenses
    createResourceGroup_resourceGroupTags,

    -- * Destructuring the Response
    CreateResourceGroupResponse (..),
    newCreateResourceGroupResponse,

    -- * Response Lenses
    createResourceGroupResponse_httpStatus,
    createResourceGroupResponse_resourceGroupArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResourceGroup' smart constructor.
data CreateResourceGroup = CreateResourceGroup'
  { -- | A collection of keys and an array of possible values,
    -- \'[{\"key\":\"key1\",\"values\":[\"Value1\",\"Value2\"]},{\"key\":\"Key2\",\"values\":[\"Value3\"]}]\'.
    --
    -- For example,\'[{\"key\":\"Name\",\"values\":[\"TestEC2Instance\"]}]\'.
    resourceGroupTags :: Prelude.NonEmpty ResourceGroupTag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupTags', 'createResourceGroup_resourceGroupTags' - A collection of keys and an array of possible values,
-- \'[{\"key\":\"key1\",\"values\":[\"Value1\",\"Value2\"]},{\"key\":\"Key2\",\"values\":[\"Value3\"]}]\'.
--
-- For example,\'[{\"key\":\"Name\",\"values\":[\"TestEC2Instance\"]}]\'.
newCreateResourceGroup ::
  -- | 'resourceGroupTags'
  Prelude.NonEmpty ResourceGroupTag ->
  CreateResourceGroup
newCreateResourceGroup pResourceGroupTags_ =
  CreateResourceGroup'
    { resourceGroupTags =
        Lens.coerced Lens.# pResourceGroupTags_
    }

-- | A collection of keys and an array of possible values,
-- \'[{\"key\":\"key1\",\"values\":[\"Value1\",\"Value2\"]},{\"key\":\"Key2\",\"values\":[\"Value3\"]}]\'.
--
-- For example,\'[{\"key\":\"Name\",\"values\":[\"TestEC2Instance\"]}]\'.
createResourceGroup_resourceGroupTags :: Lens.Lens' CreateResourceGroup (Prelude.NonEmpty ResourceGroupTag)
createResourceGroup_resourceGroupTags = Lens.lens (\CreateResourceGroup' {resourceGroupTags} -> resourceGroupTags) (\s@CreateResourceGroup' {} a -> s {resourceGroupTags = a} :: CreateResourceGroup) Prelude.. Lens.coerced

instance Core.AWSRequest CreateResourceGroup where
  type
    AWSResponse CreateResourceGroup =
      CreateResourceGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "resourceGroupArn")
      )

instance Prelude.Hashable CreateResourceGroup where
  hashWithSalt _salt CreateResourceGroup' {..} =
    _salt `Prelude.hashWithSalt` resourceGroupTags

instance Prelude.NFData CreateResourceGroup where
  rnf CreateResourceGroup' {..} =
    Prelude.rnf resourceGroupTags

instance Data.ToHeaders CreateResourceGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.CreateResourceGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResourceGroup where
  toJSON CreateResourceGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceGroupTags" Data..= resourceGroupTags)
          ]
      )

instance Data.ToPath CreateResourceGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateResourceGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceGroupResponse' smart constructor.
data CreateResourceGroupResponse = CreateResourceGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN that specifies the resource group that is created.
    resourceGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createResourceGroupResponse_httpStatus' - The response's http status code.
--
-- 'resourceGroupArn', 'createResourceGroupResponse_resourceGroupArn' - The ARN that specifies the resource group that is created.
newCreateResourceGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceGroupArn'
  Prelude.Text ->
  CreateResourceGroupResponse
newCreateResourceGroupResponse
  pHttpStatus_
  pResourceGroupArn_ =
    CreateResourceGroupResponse'
      { httpStatus =
          pHttpStatus_,
        resourceGroupArn = pResourceGroupArn_
      }

-- | The response's http status code.
createResourceGroupResponse_httpStatus :: Lens.Lens' CreateResourceGroupResponse Prelude.Int
createResourceGroupResponse_httpStatus = Lens.lens (\CreateResourceGroupResponse' {httpStatus} -> httpStatus) (\s@CreateResourceGroupResponse' {} a -> s {httpStatus = a} :: CreateResourceGroupResponse)

-- | The ARN that specifies the resource group that is created.
createResourceGroupResponse_resourceGroupArn :: Lens.Lens' CreateResourceGroupResponse Prelude.Text
createResourceGroupResponse_resourceGroupArn = Lens.lens (\CreateResourceGroupResponse' {resourceGroupArn} -> resourceGroupArn) (\s@CreateResourceGroupResponse' {} a -> s {resourceGroupArn = a} :: CreateResourceGroupResponse)

instance Prelude.NFData CreateResourceGroupResponse where
  rnf CreateResourceGroupResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf resourceGroupArn
