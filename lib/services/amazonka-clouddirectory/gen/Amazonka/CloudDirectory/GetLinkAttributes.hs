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
-- Module      : Amazonka.CloudDirectory.GetLinkAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes that are associated with a typed link.
module Amazonka.CloudDirectory.GetLinkAttributes
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
getLinkAttributes_attributeNames = Lens.lens (\GetLinkAttributes' {attributeNames} -> attributeNames) (\s@GetLinkAttributes' {} a -> s {attributeNames = a} :: GetLinkAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest GetLinkAttributes where
  type
    AWSResponse GetLinkAttributes =
      GetLinkAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLinkAttributesResponse'
            Prelude.<$> (x Data..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLinkAttributes where
  hashWithSalt _salt GetLinkAttributes' {..} =
    _salt `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` typedLinkSpecifier
      `Prelude.hashWithSalt` attributeNames

instance Prelude.NFData GetLinkAttributes where
  rnf GetLinkAttributes' {..} =
    Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf typedLinkSpecifier
      `Prelude.seq` Prelude.rnf attributeNames

instance Data.ToHeaders GetLinkAttributes where
  toHeaders GetLinkAttributes' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON GetLinkAttributes where
  toJSON GetLinkAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConsistencyLevel" Data..=)
              Prelude.<$> consistencyLevel,
            Prelude.Just
              ("TypedLinkSpecifier" Data..= typedLinkSpecifier),
            Prelude.Just
              ("AttributeNames" Data..= attributeNames)
          ]
      )

instance Data.ToPath GetLinkAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/attributes/get"

instance Data.ToQuery GetLinkAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLinkAttributesResponse' smart constructor.
data GetLinkAttributesResponse = GetLinkAttributesResponse'
  { -- | The attributes that are associated with the typed link.
    attributes :: Prelude.Maybe [AttributeKeyAndValue],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
getLinkAttributesResponse_attributes = Lens.lens (\GetLinkAttributesResponse' {attributes} -> attributes) (\s@GetLinkAttributesResponse' {} a -> s {attributes = a} :: GetLinkAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLinkAttributesResponse_httpStatus :: Lens.Lens' GetLinkAttributesResponse Prelude.Int
getLinkAttributesResponse_httpStatus = Lens.lens (\GetLinkAttributesResponse' {httpStatus} -> httpStatus) (\s@GetLinkAttributesResponse' {} a -> s {httpStatus = a} :: GetLinkAttributesResponse)

instance Prelude.NFData GetLinkAttributesResponse where
  rnf GetLinkAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
