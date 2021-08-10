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
-- Module      : Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows network ingress to a cache security group. Applications using
-- ElastiCache must be running on Amazon EC2, and Amazon EC2 security
-- groups are used as the authorization mechanism.
--
-- You cannot authorize ingress from an Amazon EC2 security group in one
-- region to an ElastiCache cluster in another region.
module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
  ( -- * Creating a Request
    AuthorizeCacheSecurityGroupIngress (..),
    newAuthorizeCacheSecurityGroupIngress,

    -- * Request Lenses
    authorizeCacheSecurityGroupIngress_cacheSecurityGroupName,
    authorizeCacheSecurityGroupIngress_eC2SecurityGroupName,
    authorizeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId,

    -- * Destructuring the Response
    AuthorizeCacheSecurityGroupIngressResponse (..),
    newAuthorizeCacheSecurityGroupIngressResponse,

    -- * Response Lenses
    authorizeCacheSecurityGroupIngressResponse_cacheSecurityGroup,
    authorizeCacheSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AuthorizeCacheSecurityGroupIngress operation.
--
-- /See:/ 'newAuthorizeCacheSecurityGroupIngress' smart constructor.
data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress'
  { -- | The cache security group that allows network ingress.
    cacheSecurityGroupName :: Prelude.Text,
    -- | The Amazon EC2 security group to be authorized for ingress to the cache
    -- security group.
    eC2SecurityGroupName :: Prelude.Text,
    -- | The AWS account number of the Amazon EC2 security group owner. Note that
    -- this is not the same thing as an AWS access key ID - you must provide a
    -- valid AWS account number for this parameter.
    eC2SecurityGroupOwnerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeCacheSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroupName', 'authorizeCacheSecurityGroupIngress_cacheSecurityGroupName' - The cache security group that allows network ingress.
--
-- 'eC2SecurityGroupName', 'authorizeCacheSecurityGroupIngress_eC2SecurityGroupName' - The Amazon EC2 security group to be authorized for ingress to the cache
-- security group.
--
-- 'eC2SecurityGroupOwnerId', 'authorizeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId' - The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
newAuthorizeCacheSecurityGroupIngress ::
  -- | 'cacheSecurityGroupName'
  Prelude.Text ->
  -- | 'eC2SecurityGroupName'
  Prelude.Text ->
  -- | 'eC2SecurityGroupOwnerId'
  Prelude.Text ->
  AuthorizeCacheSecurityGroupIngress
newAuthorizeCacheSecurityGroupIngress
  pCacheSecurityGroupName_
  pEC2SecurityGroupName_
  pEC2SecurityGroupOwnerId_ =
    AuthorizeCacheSecurityGroupIngress'
      { cacheSecurityGroupName =
          pCacheSecurityGroupName_,
        eC2SecurityGroupName =
          pEC2SecurityGroupName_,
        eC2SecurityGroupOwnerId =
          pEC2SecurityGroupOwnerId_
      }

-- | The cache security group that allows network ingress.
authorizeCacheSecurityGroupIngress_cacheSecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Prelude.Text
authorizeCacheSecurityGroupIngress_cacheSecurityGroupName = Lens.lens (\AuthorizeCacheSecurityGroupIngress' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@AuthorizeCacheSecurityGroupIngress' {} a -> s {cacheSecurityGroupName = a} :: AuthorizeCacheSecurityGroupIngress)

-- | The Amazon EC2 security group to be authorized for ingress to the cache
-- security group.
authorizeCacheSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Prelude.Text
authorizeCacheSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\AuthorizeCacheSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@AuthorizeCacheSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: AuthorizeCacheSecurityGroupIngress)

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
authorizeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeCacheSecurityGroupIngress Prelude.Text
authorizeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\AuthorizeCacheSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@AuthorizeCacheSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: AuthorizeCacheSecurityGroupIngress)

instance
  Core.AWSRequest
    AuthorizeCacheSecurityGroupIngress
  where
  type
    AWSResponse AuthorizeCacheSecurityGroupIngress =
      AuthorizeCacheSecurityGroupIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AuthorizeCacheSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeCacheSecurityGroupIngressResponse'
            Prelude.<$> (x Core..@? "CacheSecurityGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AuthorizeCacheSecurityGroupIngress

instance
  Prelude.NFData
    AuthorizeCacheSecurityGroupIngress

instance
  Core.ToHeaders
    AuthorizeCacheSecurityGroupIngress
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    AuthorizeCacheSecurityGroupIngress
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AuthorizeCacheSecurityGroupIngress
  where
  toQuery AuthorizeCacheSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "AuthorizeCacheSecurityGroupIngress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSecurityGroupName"
          Core.=: cacheSecurityGroupName,
        "EC2SecurityGroupName" Core.=: eC2SecurityGroupName,
        "EC2SecurityGroupOwnerId"
          Core.=: eC2SecurityGroupOwnerId
      ]

-- | /See:/ 'newAuthorizeCacheSecurityGroupIngressResponse' smart constructor.
data AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Prelude.Maybe CacheSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeCacheSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroup', 'authorizeCacheSecurityGroupIngressResponse_cacheSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'authorizeCacheSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newAuthorizeCacheSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AuthorizeCacheSecurityGroupIngressResponse
newAuthorizeCacheSecurityGroupIngressResponse
  pHttpStatus_ =
    AuthorizeCacheSecurityGroupIngressResponse'
      { cacheSecurityGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
authorizeCacheSecurityGroupIngressResponse_cacheSecurityGroup :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse (Prelude.Maybe CacheSecurityGroup)
authorizeCacheSecurityGroupIngressResponse_cacheSecurityGroup = Lens.lens (\AuthorizeCacheSecurityGroupIngressResponse' {cacheSecurityGroup} -> cacheSecurityGroup) (\s@AuthorizeCacheSecurityGroupIngressResponse' {} a -> s {cacheSecurityGroup = a} :: AuthorizeCacheSecurityGroupIngressResponse)

-- | The response's http status code.
authorizeCacheSecurityGroupIngressResponse_httpStatus :: Lens.Lens' AuthorizeCacheSecurityGroupIngressResponse Prelude.Int
authorizeCacheSecurityGroupIngressResponse_httpStatus = Lens.lens (\AuthorizeCacheSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@AuthorizeCacheSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: AuthorizeCacheSecurityGroupIngressResponse)

instance
  Prelude.NFData
    AuthorizeCacheSecurityGroupIngressResponse
