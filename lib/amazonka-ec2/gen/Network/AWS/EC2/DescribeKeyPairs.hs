{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeKeyPairs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified key pairs or all of your key pairs.
--
-- For more information about key pairs, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeKeyPairs
  ( -- * Creating a request
    DescribeKeyPairs (..),
    mkDescribeKeyPairs,

    -- ** Request lenses
    dkpFilters,
    dkpKeyPairIds,
    dkpKeyNames,
    dkpDryRun,

    -- * Destructuring the response
    DescribeKeyPairsResponse (..),
    mkDescribeKeyPairsResponse,

    -- ** Response lenses
    dkprsKeyPairs,
    dkprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeKeyPairs' smart constructor.
data DescribeKeyPairs = DescribeKeyPairs'
  { -- | The filters.
    --
    --
    --     * @key-pair-id@ - The ID of the key pair.
    --
    --
    --     * @fingerprint@ - The fingerprint of the key pair.
    --
    --
    --     * @key-name@ - The name of the key pair.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    filters :: Lude.Maybe [Filter],
    -- | The IDs of the key pairs.
    keyPairIds :: Lude.Maybe [Lude.Text],
    -- | The key pair names.
    --
    -- Default: Describes all your key pairs.
    keyNames :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeKeyPairs' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
--
--
--     * @key-pair-id@ - The ID of the key pair.
--
--
--     * @fingerprint@ - The fingerprint of the key pair.
--
--
--     * @key-name@ - The name of the key pair.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
-- * 'keyPairIds' - The IDs of the key pairs.
-- * 'keyNames' - The key pair names.
--
-- Default: Describes all your key pairs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeKeyPairs ::
  DescribeKeyPairs
mkDescribeKeyPairs =
  DescribeKeyPairs'
    { filters = Lude.Nothing,
      keyPairIds = Lude.Nothing,
      keyNames = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The filters.
--
--
--     * @key-pair-id@ - The ID of the key pair.
--
--
--     * @fingerprint@ - The fingerprint of the key pair.
--
--
--     * @key-name@ - The name of the key pair.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpFilters :: Lens.Lens' DescribeKeyPairs (Lude.Maybe [Filter])
dkpFilters = Lens.lens (filters :: DescribeKeyPairs -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeKeyPairs)
{-# DEPRECATED dkpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the key pairs.
--
-- /Note:/ Consider using 'keyPairIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyPairIds :: Lens.Lens' DescribeKeyPairs (Lude.Maybe [Lude.Text])
dkpKeyPairIds = Lens.lens (keyPairIds :: DescribeKeyPairs -> Lude.Maybe [Lude.Text]) (\s a -> s {keyPairIds = a} :: DescribeKeyPairs)
{-# DEPRECATED dkpKeyPairIds "Use generic-lens or generic-optics with 'keyPairIds' instead." #-}

-- | The key pair names.
--
-- Default: Describes all your key pairs.
--
-- /Note:/ Consider using 'keyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyNames :: Lens.Lens' DescribeKeyPairs (Lude.Maybe [Lude.Text])
dkpKeyNames = Lens.lens (keyNames :: DescribeKeyPairs -> Lude.Maybe [Lude.Text]) (\s a -> s {keyNames = a} :: DescribeKeyPairs)
{-# DEPRECATED dkpKeyNames "Use generic-lens or generic-optics with 'keyNames' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpDryRun :: Lens.Lens' DescribeKeyPairs (Lude.Maybe Lude.Bool)
dkpDryRun = Lens.lens (dryRun :: DescribeKeyPairs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeKeyPairs)
{-# DEPRECATED dkpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeKeyPairs where
  type Rs DescribeKeyPairs = DescribeKeyPairsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeKeyPairsResponse'
            Lude.<$> ( x Lude..@? "keySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeKeyPairs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeKeyPairs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeKeyPairs where
  toQuery DescribeKeyPairs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeKeyPairs" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "KeyPairId" Lude.<$> keyPairIds),
        Lude.toQuery (Lude.toQueryList "KeyName" Lude.<$> keyNames),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeKeyPairsResponse' smart constructor.
data DescribeKeyPairsResponse = DescribeKeyPairsResponse'
  { -- | Information about the key pairs.
    keyPairs :: Lude.Maybe [KeyPairInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeKeyPairsResponse' with the minimum fields required to make a request.
--
-- * 'keyPairs' - Information about the key pairs.
-- * 'responseStatus' - The response status code.
mkDescribeKeyPairsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeKeyPairsResponse
mkDescribeKeyPairsResponse pResponseStatus_ =
  DescribeKeyPairsResponse'
    { keyPairs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the key pairs.
--
-- /Note:/ Consider using 'keyPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprsKeyPairs :: Lens.Lens' DescribeKeyPairsResponse (Lude.Maybe [KeyPairInfo])
dkprsKeyPairs = Lens.lens (keyPairs :: DescribeKeyPairsResponse -> Lude.Maybe [KeyPairInfo]) (\s a -> s {keyPairs = a} :: DescribeKeyPairsResponse)
{-# DEPRECATED dkprsKeyPairs "Use generic-lens or generic-optics with 'keyPairs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprsResponseStatus :: Lens.Lens' DescribeKeyPairsResponse Lude.Int
dkprsResponseStatus = Lens.lens (responseStatus :: DescribeKeyPairsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeKeyPairsResponse)
{-# DEPRECATED dkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
