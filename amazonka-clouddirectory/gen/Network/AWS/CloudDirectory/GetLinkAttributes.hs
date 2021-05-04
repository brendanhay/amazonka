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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLinkAttributes' smart constructor.
data GetLinkAttributes = GetLinkAttributes'
  { -- | The consistency level at which to retrieve the attributes on a typed
    -- link.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the typed link resides. For more information, see arns or
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    directoryArn :: Prelude.Text,
    -- | Allows a typed link specifier to be accepted as input.
    typedLinkSpecifier :: TypedLinkSpecifier,
    -- | A list of attribute names whose values will be retrieved.
    attributeNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  GetLinkAttributes
newGetLinkAttributes
  pDirectoryArn_
  pTypedLinkSpecifier_ =
    GetLinkAttributes'
      { consistencyLevel =
          Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        typedLinkSpecifier = pTypedLinkSpecifier_,
        attributeNames = Prelude.mempty
      }

-- | The consistency level at which to retrieve the attributes on a typed
-- link.
getLinkAttributes_consistencyLevel :: Lens.Lens' GetLinkAttributes (Prelude.Maybe ConsistencyLevel)
getLinkAttributes_consistencyLevel = Lens.lens (\GetLinkAttributes' {consistencyLevel} -> consistencyLevel) (\s@GetLinkAttributes' {} a -> s {consistencyLevel = a} :: GetLinkAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the typed link resides. For more information, see arns or
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
getLinkAttributes_directoryArn :: Lens.Lens' GetLinkAttributes Prelude.Text
getLinkAttributes_directoryArn = Lens.lens (\GetLinkAttributes' {directoryArn} -> directoryArn) (\s@GetLinkAttributes' {} a -> s {directoryArn = a} :: GetLinkAttributes)

-- | Allows a typed link specifier to be accepted as input.
getLinkAttributes_typedLinkSpecifier :: Lens.Lens' GetLinkAttributes TypedLinkSpecifier
getLinkAttributes_typedLinkSpecifier = Lens.lens (\GetLinkAttributes' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@GetLinkAttributes' {} a -> s {typedLinkSpecifier = a} :: GetLinkAttributes)

-- | A list of attribute names whose values will be retrieved.
getLinkAttributes_attributeNames :: Lens.Lens' GetLinkAttributes [Prelude.Text]
getLinkAttributes_attributeNames = Lens.lens (\GetLinkAttributes' {attributeNames} -> attributeNames) (\s@GetLinkAttributes' {} a -> s {attributeNames = a} :: GetLinkAttributes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest GetLinkAttributes where
  type Rs GetLinkAttributes = GetLinkAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLinkAttributesResponse'
            Prelude.<$> ( x Prelude..?> "Attributes"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLinkAttributes

instance Prelude.NFData GetLinkAttributes

instance Prelude.ToHeaders GetLinkAttributes where
  toHeaders GetLinkAttributes' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON GetLinkAttributes where
  toJSON GetLinkAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ConsistencyLevel" Prelude..=)
              Prelude.<$> consistencyLevel,
            Prelude.Just
              ("TypedLinkSpecifier" Prelude..= typedLinkSpecifier),
            Prelude.Just
              ("AttributeNames" Prelude..= attributeNames)
          ]
      )

instance Prelude.ToPath GetLinkAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/attributes/get"

instance Prelude.ToQuery GetLinkAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLinkAttributesResponse' smart constructor.
data GetLinkAttributesResponse = GetLinkAttributesResponse'
  { -- | The attributes that are associated with the typed link.
    attributes :: Prelude.Maybe [AttributeKeyAndValue],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetLinkAttributesResponse
newGetLinkAttributesResponse pHttpStatus_ =
  GetLinkAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes that are associated with the typed link.
getLinkAttributesResponse_attributes :: Lens.Lens' GetLinkAttributesResponse (Prelude.Maybe [AttributeKeyAndValue])
getLinkAttributesResponse_attributes = Lens.lens (\GetLinkAttributesResponse' {attributes} -> attributes) (\s@GetLinkAttributesResponse' {} a -> s {attributes = a} :: GetLinkAttributesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getLinkAttributesResponse_httpStatus :: Lens.Lens' GetLinkAttributesResponse Prelude.Int
getLinkAttributesResponse_httpStatus = Lens.lens (\GetLinkAttributesResponse' {httpStatus} -> httpStatus) (\s@GetLinkAttributesResponse' {} a -> s {httpStatus = a} :: GetLinkAttributesResponse)

instance Prelude.NFData GetLinkAttributesResponse
