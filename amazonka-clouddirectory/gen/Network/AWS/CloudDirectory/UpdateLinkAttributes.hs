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
-- Module      : Network.AWS.CloudDirectory.UpdateLinkAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given typed link’s attributes. Attributes to be updated must
-- not contribute to the typed link’s identity, as defined by its
-- @IdentityAttributeOrder@.
module Network.AWS.CloudDirectory.UpdateLinkAttributes
  ( -- * Creating a Request
    UpdateLinkAttributes (..),
    newUpdateLinkAttributes,

    -- * Request Lenses
    updateLinkAttributes_directoryArn,
    updateLinkAttributes_typedLinkSpecifier,
    updateLinkAttributes_attributeUpdates,

    -- * Destructuring the Response
    UpdateLinkAttributesResponse (..),
    newUpdateLinkAttributesResponse,

    -- * Response Lenses
    updateLinkAttributesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLinkAttributes' smart constructor.
data UpdateLinkAttributes = UpdateLinkAttributes'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the updated typed link resides. For more information, see arns or
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    directoryArn :: Core.Text,
    -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: TypedLinkSpecifier,
    -- | The attributes update structure.
    attributeUpdates :: [LinkAttributeUpdate]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateLinkAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'updateLinkAttributes_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the updated typed link resides. For more information, see arns or
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'typedLinkSpecifier', 'updateLinkAttributes_typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- 'attributeUpdates', 'updateLinkAttributes_attributeUpdates' - The attributes update structure.
newUpdateLinkAttributes ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  UpdateLinkAttributes
newUpdateLinkAttributes
  pDirectoryArn_
  pTypedLinkSpecifier_ =
    UpdateLinkAttributes'
      { directoryArn =
          pDirectoryArn_,
        typedLinkSpecifier = pTypedLinkSpecifier_,
        attributeUpdates = Core.mempty
      }

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the updated typed link resides. For more information, see arns or
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
updateLinkAttributes_directoryArn :: Lens.Lens' UpdateLinkAttributes Core.Text
updateLinkAttributes_directoryArn = Lens.lens (\UpdateLinkAttributes' {directoryArn} -> directoryArn) (\s@UpdateLinkAttributes' {} a -> s {directoryArn = a} :: UpdateLinkAttributes)

-- | Allows a typed link specifier to be accepted as input.
updateLinkAttributes_typedLinkSpecifier :: Lens.Lens' UpdateLinkAttributes TypedLinkSpecifier
updateLinkAttributes_typedLinkSpecifier = Lens.lens (\UpdateLinkAttributes' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@UpdateLinkAttributes' {} a -> s {typedLinkSpecifier = a} :: UpdateLinkAttributes)

-- | The attributes update structure.
updateLinkAttributes_attributeUpdates :: Lens.Lens' UpdateLinkAttributes [LinkAttributeUpdate]
updateLinkAttributes_attributeUpdates = Lens.lens (\UpdateLinkAttributes' {attributeUpdates} -> attributeUpdates) (\s@UpdateLinkAttributes' {} a -> s {attributeUpdates = a} :: UpdateLinkAttributes) Core.. Lens._Coerce

instance Core.AWSRequest UpdateLinkAttributes where
  type
    AWSResponse UpdateLinkAttributes =
      UpdateLinkAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLinkAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateLinkAttributes

instance Core.NFData UpdateLinkAttributes

instance Core.ToHeaders UpdateLinkAttributes where
  toHeaders UpdateLinkAttributes' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON UpdateLinkAttributes where
  toJSON UpdateLinkAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("TypedLinkSpecifier" Core..= typedLinkSpecifier),
            Core.Just
              ("AttributeUpdates" Core..= attributeUpdates)
          ]
      )

instance Core.ToPath UpdateLinkAttributes where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/typedlink/attributes/update"

instance Core.ToQuery UpdateLinkAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateLinkAttributesResponse' smart constructor.
data UpdateLinkAttributesResponse = UpdateLinkAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateLinkAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLinkAttributesResponse_httpStatus' - The response's http status code.
newUpdateLinkAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateLinkAttributesResponse
newUpdateLinkAttributesResponse pHttpStatus_ =
  UpdateLinkAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateLinkAttributesResponse_httpStatus :: Lens.Lens' UpdateLinkAttributesResponse Core.Int
updateLinkAttributesResponse_httpStatus = Lens.lens (\UpdateLinkAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateLinkAttributesResponse' {} a -> s {httpStatus = a} :: UpdateLinkAttributesResponse)

instance Core.NFData UpdateLinkAttributesResponse
