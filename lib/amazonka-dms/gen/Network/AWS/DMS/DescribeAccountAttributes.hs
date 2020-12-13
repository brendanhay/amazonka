{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the AWS DMS attributes for a customer account. These attributes include AWS DMS quotas for the account and a unique account identifier in a particular DMS region. DMS quotas include a list of resource quotas supported by the account, such as the number of replication instances allowed. The description for each resource quota, includes the quota name, current usage toward that quota, and the quota's maximum value. DMS uses the unique account identifier to name each artifact used by DMS in the given region.
--
-- This command does not take any parameters.
module Network.AWS.DMS.DescribeAccountAttributes
  ( -- * Creating a request
    DescribeAccountAttributes (..),
    mkDescribeAccountAttributes,

    -- * Destructuring the response
    DescribeAccountAttributesResponse (..),
    mkDescribeAccountAttributesResponse,

    -- ** Response lenses
    daarsAccountQuotas,
    daarsUniqueAccountIdentifier,
    daarsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
mkDescribeAccountAttributes ::
  DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Lude.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Lude.<$> (x Lude..?> "AccountQuotas" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UniqueAccountIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeAccountAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAccountAttributes where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeAccountAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountAttributes where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | Account quota information.
    accountQuotas :: Lude.Maybe [AccountQuota],
    -- | A unique AWS DMS identifier for an account in a particular AWS Region. The value of this identifier has the following format: @c99999999999@ . DMS uses this identifier to name artifacts. For example, DMS uses this identifier to name the default Amazon S3 bucket for storing task assessment reports in a given AWS Region. The format of this S3 bucket name is the following: @dms-/AccountNumber/ -/UniqueAccountIdentifier/ .@ Here is an example name for this default S3 bucket: @dms-111122223333-c44445555666@ .
    uniqueAccountIdentifier :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- * 'accountQuotas' - Account quota information.
-- * 'uniqueAccountIdentifier' - A unique AWS DMS identifier for an account in a particular AWS Region. The value of this identifier has the following format: @c99999999999@ . DMS uses this identifier to name artifacts. For example, DMS uses this identifier to name the default Amazon S3 bucket for storing task assessment reports in a given AWS Region. The format of this S3 bucket name is the following: @dms-/AccountNumber/ -/UniqueAccountIdentifier/ .@ Here is an example name for this default S3 bucket: @dms-111122223333-c44445555666@ .
-- * 'responseStatus' - The response status code.
mkDescribeAccountAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    { accountQuotas = Lude.Nothing,
      uniqueAccountIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Account quota information.
--
-- /Note:/ Consider using 'accountQuotas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAccountQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Lude.Maybe [AccountQuota])
daarsAccountQuotas = Lens.lens (accountQuotas :: DescribeAccountAttributesResponse -> Lude.Maybe [AccountQuota]) (\s a -> s {accountQuotas = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsAccountQuotas "Use generic-lens or generic-optics with 'accountQuotas' instead." #-}

-- | A unique AWS DMS identifier for an account in a particular AWS Region. The value of this identifier has the following format: @c99999999999@ . DMS uses this identifier to name artifacts. For example, DMS uses this identifier to name the default Amazon S3 bucket for storing task assessment reports in a given AWS Region. The format of this S3 bucket name is the following: @dms-/AccountNumber/ -/UniqueAccountIdentifier/ .@ Here is an example name for this default S3 bucket: @dms-111122223333-c44445555666@ .
--
-- /Note:/ Consider using 'uniqueAccountIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsUniqueAccountIdentifier :: Lens.Lens' DescribeAccountAttributesResponse (Lude.Maybe Lude.Text)
daarsUniqueAccountIdentifier = Lens.lens (uniqueAccountIdentifier :: DescribeAccountAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {uniqueAccountIdentifier = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsUniqueAccountIdentifier "Use generic-lens or generic-optics with 'uniqueAccountIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAccountAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
