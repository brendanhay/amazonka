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
-- Module      : Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a cache security group. Use this operation to
-- disallow access from an Amazon EC2 security group that had been
-- previously authorized.
module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
  ( -- * Creating a Request
    RevokeCacheSecurityGroupIngress (..),
    newRevokeCacheSecurityGroupIngress,

    -- * Request Lenses
    revokeCacheSecurityGroupIngress_cacheSecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId,

    -- * Destructuring the Response
    RevokeCacheSecurityGroupIngressResponse (..),
    newRevokeCacheSecurityGroupIngressResponse,

    -- * Response Lenses
    revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup,
    revokeCacheSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RevokeCacheSecurityGroupIngress@ operation.
--
-- /See:/ 'newRevokeCacheSecurityGroupIngress' smart constructor.
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress'
  { -- | The name of the cache security group to revoke ingress from.
    cacheSecurityGroupName :: Core.Text,
    -- | The name of the Amazon EC2 security group to revoke access from.
    eC2SecurityGroupName :: Core.Text,
    -- | The AWS account number of the Amazon EC2 security group owner. Note that
    -- this is not the same thing as an AWS access key ID - you must provide a
    -- valid AWS account number for this parameter.
    eC2SecurityGroupOwnerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeCacheSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroupName', 'revokeCacheSecurityGroupIngress_cacheSecurityGroupName' - The name of the cache security group to revoke ingress from.
--
-- 'eC2SecurityGroupName', 'revokeCacheSecurityGroupIngress_eC2SecurityGroupName' - The name of the Amazon EC2 security group to revoke access from.
--
-- 'eC2SecurityGroupOwnerId', 'revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId' - The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
newRevokeCacheSecurityGroupIngress ::
  -- | 'cacheSecurityGroupName'
  Core.Text ->
  -- | 'eC2SecurityGroupName'
  Core.Text ->
  -- | 'eC2SecurityGroupOwnerId'
  Core.Text ->
  RevokeCacheSecurityGroupIngress
newRevokeCacheSecurityGroupIngress
  pCacheSecurityGroupName_
  pEC2SecurityGroupName_
  pEC2SecurityGroupOwnerId_ =
    RevokeCacheSecurityGroupIngress'
      { cacheSecurityGroupName =
          pCacheSecurityGroupName_,
        eC2SecurityGroupName =
          pEC2SecurityGroupName_,
        eC2SecurityGroupOwnerId =
          pEC2SecurityGroupOwnerId_
      }

-- | The name of the cache security group to revoke ingress from.
revokeCacheSecurityGroupIngress_cacheSecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Core.Text
revokeCacheSecurityGroupIngress_cacheSecurityGroupName = Lens.lens (\RevokeCacheSecurityGroupIngress' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@RevokeCacheSecurityGroupIngress' {} a -> s {cacheSecurityGroupName = a} :: RevokeCacheSecurityGroupIngress)

-- | The name of the Amazon EC2 security group to revoke access from.
revokeCacheSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Core.Text
revokeCacheSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\RevokeCacheSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@RevokeCacheSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: RevokeCacheSecurityGroupIngress)

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' RevokeCacheSecurityGroupIngress Core.Text
revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\RevokeCacheSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@RevokeCacheSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: RevokeCacheSecurityGroupIngress)

instance
  Core.AWSRequest
    RevokeCacheSecurityGroupIngress
  where
  type
    AWSResponse RevokeCacheSecurityGroupIngress =
      RevokeCacheSecurityGroupIngressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RevokeCacheSecurityGroupIngressResult"
      ( \s h x ->
          RevokeCacheSecurityGroupIngressResponse'
            Core.<$> (x Core..@? "CacheSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RevokeCacheSecurityGroupIngress

instance Core.NFData RevokeCacheSecurityGroupIngress

instance
  Core.ToHeaders
    RevokeCacheSecurityGroupIngress
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RevokeCacheSecurityGroupIngress where
  toPath = Core.const "/"

instance Core.ToQuery RevokeCacheSecurityGroupIngress where
  toQuery RevokeCacheSecurityGroupIngress' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "RevokeCacheSecurityGroupIngress" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "CacheSecurityGroupName"
          Core.=: cacheSecurityGroupName,
        "EC2SecurityGroupName" Core.=: eC2SecurityGroupName,
        "EC2SecurityGroupOwnerId"
          Core.=: eC2SecurityGroupOwnerId
      ]

-- | /See:/ 'newRevokeCacheSecurityGroupIngressResponse' smart constructor.
data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Core.Maybe CacheSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeCacheSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroup', 'revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'revokeCacheSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newRevokeCacheSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RevokeCacheSecurityGroupIngressResponse
newRevokeCacheSecurityGroupIngressResponse
  pHttpStatus_ =
    RevokeCacheSecurityGroupIngressResponse'
      { cacheSecurityGroup =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse (Core.Maybe CacheSecurityGroup)
revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup = Lens.lens (\RevokeCacheSecurityGroupIngressResponse' {cacheSecurityGroup} -> cacheSecurityGroup) (\s@RevokeCacheSecurityGroupIngressResponse' {} a -> s {cacheSecurityGroup = a} :: RevokeCacheSecurityGroupIngressResponse)

-- | The response's http status code.
revokeCacheSecurityGroupIngressResponse_httpStatus :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse Core.Int
revokeCacheSecurityGroupIngressResponse_httpStatus = Lens.lens (\RevokeCacheSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeCacheSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: RevokeCacheSecurityGroupIngressResponse)

instance
  Core.NFData
    RevokeCacheSecurityGroupIngressResponse
