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
-- Module      : Network.AWS.Lightsail.CreateDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain resource for the specified domain (e.g., example.com).
--
-- The @create domain@ operation supports tag-based access control via
-- request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_tags,
    createDomain_domainName,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_operation,
    createDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The domain name to manage (e.g., @example.com@).
    --
    -- You cannot register a new domain name using Lightsail. You must register
    -- a domain name using Amazon Route 53 or another domain name registrar. If
    -- you have already registered your domain, you can enter its name in this
    -- parameter to manage the DNS records for that domain.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDomain_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'domainName', 'createDomain_domainName' - The domain name to manage (e.g., @example.com@).
--
-- You cannot register a new domain name using Lightsail. You must register
-- a domain name using Amazon Route 53 or another domain name registrar. If
-- you have already registered your domain, you can enter its name in this
-- parameter to manage the DNS records for that domain.
newCreateDomain ::
  -- | 'domainName'
  Core.Text ->
  CreateDomain
newCreateDomain pDomainName_ =
  CreateDomain'
    { tags = Core.Nothing,
      domainName = pDomainName_
    }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createDomain_tags :: Lens.Lens' CreateDomain (Core.Maybe [Tag])
createDomain_tags = Lens.lens (\CreateDomain' {tags} -> tags) (\s@CreateDomain' {} a -> s {tags = a} :: CreateDomain) Core.. Lens.mapping Lens._Coerce

-- | The domain name to manage (e.g., @example.com@).
--
-- You cannot register a new domain name using Lightsail. You must register
-- a domain name using Amazon Route 53 or another domain name registrar. If
-- you have already registered your domain, you can enter its name in this
-- parameter to manage the DNS records for that domain.
createDomain_domainName :: Lens.Lens' CreateDomain Core.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDomain

instance Core.NFData CreateDomain

instance Core.ToHeaders CreateDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just ("domainName" Core..= domainName)
          ]
      )

instance Core.ToPath CreateDomain where
  toPath = Core.const "/"

instance Core.ToQuery CreateDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'createDomainResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDomainResponse
newCreateDomainResponse pHttpStatus_ =
  CreateDomainResponse'
    { operation = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createDomainResponse_operation :: Lens.Lens' CreateDomainResponse (Core.Maybe Operation)
createDomainResponse_operation = Lens.lens (\CreateDomainResponse' {operation} -> operation) (\s@CreateDomainResponse' {} a -> s {operation = a} :: CreateDomainResponse)

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Core.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

instance Core.NFData CreateDomainResponse
