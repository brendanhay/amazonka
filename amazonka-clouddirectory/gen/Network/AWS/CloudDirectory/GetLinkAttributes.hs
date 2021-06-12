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
-- Module      : Network.AWS.CloudDirectory.GetLinkAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes that are associated with a typed link.
module Network.AWS.CloudDirectory.GetLinkAttributes
  ( -- * Creating a Request
    GetLinkAttributes (..),
    newGetLinkAttributes,

    -- * Request Lenses
    getLinkAttributes_consistencyLevel,
    getLinkAttributes_directoryArn,
    getLinkAttributes_typedLinkSpecifier,
    getLinkAttributes_attributeNames,

    -- * Destructuring the Response
    GetLinkAttributesResponse (..),
    newGetLinkAttributesResponse,

    -- * Response Lenses
    getLinkAttributesResponse_attributes,
    getLinkAttributesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLinkAttributes' smart constructor.
data GetLinkAttributes = GetLinkAttributes'
  { -- | The consistency level at which to retrieve the attributes on a typed
    -- link.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the typed link resides. For more information, see arns or
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    directoryArn :: Core.Text,
    -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: TypedLinkSpecifier,
    -- | A list of attribute names whose values will be retrieved.
    attributeNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLinkAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'getLinkAttributes_consistencyLevel' - The consistency level at which to retrieve the attributes on a typed
-- link.
--
-- 'directoryArn', 'getLinkAttributes_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the typed link resides. For more information, see arns or
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'typedLinkSpecifier', 'getLinkAttributes_typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- 'attributeNames', 'getLinkAttributes_attributeNames' - A list of attribute names whose values will be retrieved.
newGetLinkAttributes ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  GetLinkAttributes
newGetLinkAttributes
  pDirectoryArn_
  pTypedLinkSpecifier_ =
    GetLinkAttributes'
      { consistencyLevel = Core.Nothing,
        directoryArn = pDirectoryArn_,
        typedLinkSpecifier = pTypedLinkSpecifier_,
        attributeNames = Core.mempty
      }

-- | The consistency level at which to retrieve the attributes on a typed
-- link.
getLinkAttributes_consistencyLevel :: Lens.Lens' GetLinkAttributes (Core.Maybe ConsistencyLevel)
getLinkAttributes_consistencyLevel = Lens.lens (\GetLinkAttributes' {consistencyLevel} -> consistencyLevel) (\s@GetLinkAttributes' {} a -> s {consistencyLevel = a} :: GetLinkAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the typed link resides. For more information, see arns or
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
getLinkAttributes_directoryArn :: Lens.Lens' GetLinkAttributes Core.Text
getLinkAttributes_directoryArn = Lens.lens (\GetLinkAttributes' {directoryArn} -> directoryArn) (\s@GetLinkAttributes' {} a -> s {directoryArn = a} :: GetLinkAttributes)

-- | Allows a typed link specifier to be accepted as input.
getLinkAttributes_typedLinkSpecifier :: Lens.Lens' GetLinkAttributes TypedLinkSpecifier
getLinkAttributes_typedLinkSpecifier = Lens.lens (\GetLinkAttributes' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@GetLinkAttributes' {} a -> s {typedLinkSpecifier = a} :: GetLinkAttributes)

-- | A list of attribute names whose values will be retrieved.
getLinkAttributes_attributeNames :: Lens.Lens' GetLinkAttributes [Core.Text]
getLinkAttributes_attributeNames = Lens.lens (\GetLinkAttributes' {attributeNames} -> attributeNames) (\s@GetLinkAttributes' {} a -> s {attributeNames = a} :: GetLinkAttributes) Core.. Lens._Coerce

instance Core.AWSRequest GetLinkAttributes where
  type
    AWSResponse GetLinkAttributes =
      GetLinkAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLinkAttributesResponse'
            Core.<$> (x Core..?> "Attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLinkAttributes

instance Core.NFData GetLinkAttributes

instance Core.ToHeaders GetLinkAttributes where
  toHeaders GetLinkAttributes' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON GetLinkAttributes where
  toJSON GetLinkAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConsistencyLevel" Core..=)
              Core.<$> consistencyLevel,
            Core.Just
              ("TypedLinkSpecifier" Core..= typedLinkSpecifier),
            Core.Just ("AttributeNames" Core..= attributeNames)
          ]
      )

instance Core.ToPath GetLinkAttributes where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/typedlink/attributes/get"

instance Core.ToQuery GetLinkAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLinkAttributesResponse' smart constructor.
data GetLinkAttributesResponse = GetLinkAttributesResponse'
  { -- | The attributes that are associated with the typed link.
    attributes :: Core.Maybe [AttributeKeyAndValue],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLinkAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getLinkAttributesResponse_attributes' - The attributes that are associated with the typed link.
--
-- 'httpStatus', 'getLinkAttributesResponse_httpStatus' - The response's http status code.
newGetLinkAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLinkAttributesResponse
newGetLinkAttributesResponse pHttpStatus_ =
  GetLinkAttributesResponse'
    { attributes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes that are associated with the typed link.
getLinkAttributesResponse_attributes :: Lens.Lens' GetLinkAttributesResponse (Core.Maybe [AttributeKeyAndValue])
getLinkAttributesResponse_attributes = Lens.lens (\GetLinkAttributesResponse' {attributes} -> attributes) (\s@GetLinkAttributesResponse' {} a -> s {attributes = a} :: GetLinkAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getLinkAttributesResponse_httpStatus :: Lens.Lens' GetLinkAttributesResponse Core.Int
getLinkAttributesResponse_httpStatus = Lens.lens (\GetLinkAttributesResponse' {httpStatus} -> httpStatus) (\s@GetLinkAttributesResponse' {} a -> s {httpStatus = a} :: GetLinkAttributesResponse)

instance Core.NFData GetLinkAttributesResponse
