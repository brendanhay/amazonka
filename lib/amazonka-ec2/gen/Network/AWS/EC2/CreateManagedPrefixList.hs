{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed prefix list. You can specify one or more entries for the prefix list. Each entry consists of a CIDR block and an optional description.
--
-- You must specify the maximum number of entries for the prefix list. The maximum number of entries cannot be changed later.
module Network.AWS.EC2.CreateManagedPrefixList
  ( -- * Creating a request
    CreateManagedPrefixList (..),
    mkCreateManagedPrefixList,

    -- ** Request lenses
    cmplClientToken,
    cmplEntries,
    cmplTagSpecifications,
    cmplDryRun,
    cmplPrefixListName,
    cmplMaxEntries,
    cmplAddressFamily,

    -- * Destructuring the response
    CreateManagedPrefixListResponse (..),
    mkCreateManagedPrefixListResponse,

    -- ** Response lenses
    cmplrsPrefixList,
    cmplrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateManagedPrefixList' smart constructor.
data CreateManagedPrefixList = CreateManagedPrefixList'
  { clientToken ::
      Lude.Maybe Lude.Text,
    entries :: Lude.Maybe [AddPrefixListEntry],
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    dryRun :: Lude.Maybe Lude.Bool,
    prefixListName :: Lude.Text,
    maxEntries :: Lude.Int,
    addressFamily :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateManagedPrefixList' with the minimum fields required to make a request.
--
-- * 'addressFamily' - The IP address type.
--
-- Valid Values: @IPv4@ | @IPv6@
-- * 'clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Up to 255 UTF-8 characters in length.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'entries' - One or more entries for the prefix list.
-- * 'maxEntries' - The maximum number of entries for the prefix list.
-- * 'prefixListName' - A name for the prefix list.
--
-- Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
-- * 'tagSpecifications' - The tags to apply to the prefix list during creation.
mkCreateManagedPrefixList ::
  -- | 'prefixListName'
  Lude.Text ->
  -- | 'maxEntries'
  Lude.Int ->
  -- | 'addressFamily'
  Lude.Text ->
  CreateManagedPrefixList
mkCreateManagedPrefixList
  pPrefixListName_
  pMaxEntries_
  pAddressFamily_ =
    CreateManagedPrefixList'
      { clientToken = Lude.Nothing,
        entries = Lude.Nothing,
        tagSpecifications = Lude.Nothing,
        dryRun = Lude.Nothing,
        prefixListName = pPrefixListName_,
        maxEntries = pMaxEntries_,
        addressFamily = pAddressFamily_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Up to 255 UTF-8 characters in length.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplClientToken :: Lens.Lens' CreateManagedPrefixList (Lude.Maybe Lude.Text)
cmplClientToken = Lens.lens (clientToken :: CreateManagedPrefixList -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateManagedPrefixList)
{-# DEPRECATED cmplClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | One or more entries for the prefix list.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplEntries :: Lens.Lens' CreateManagedPrefixList (Lude.Maybe [AddPrefixListEntry])
cmplEntries = Lens.lens (entries :: CreateManagedPrefixList -> Lude.Maybe [AddPrefixListEntry]) (\s a -> s {entries = a} :: CreateManagedPrefixList)
{-# DEPRECATED cmplEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The tags to apply to the prefix list during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplTagSpecifications :: Lens.Lens' CreateManagedPrefixList (Lude.Maybe [TagSpecification])
cmplTagSpecifications = Lens.lens (tagSpecifications :: CreateManagedPrefixList -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateManagedPrefixList)
{-# DEPRECATED cmplTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplDryRun :: Lens.Lens' CreateManagedPrefixList (Lude.Maybe Lude.Bool)
cmplDryRun = Lens.lens (dryRun :: CreateManagedPrefixList -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateManagedPrefixList)
{-# DEPRECATED cmplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A name for the prefix list.
--
-- Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
--
-- /Note:/ Consider using 'prefixListName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplPrefixListName :: Lens.Lens' CreateManagedPrefixList Lude.Text
cmplPrefixListName = Lens.lens (prefixListName :: CreateManagedPrefixList -> Lude.Text) (\s a -> s {prefixListName = a} :: CreateManagedPrefixList)
{-# DEPRECATED cmplPrefixListName "Use generic-lens or generic-optics with 'prefixListName' instead." #-}

-- | The maximum number of entries for the prefix list.
--
-- /Note:/ Consider using 'maxEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplMaxEntries :: Lens.Lens' CreateManagedPrefixList Lude.Int
cmplMaxEntries = Lens.lens (maxEntries :: CreateManagedPrefixList -> Lude.Int) (\s a -> s {maxEntries = a} :: CreateManagedPrefixList)
{-# DEPRECATED cmplMaxEntries "Use generic-lens or generic-optics with 'maxEntries' instead." #-}

-- | The IP address type.
--
-- Valid Values: @IPv4@ | @IPv6@
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplAddressFamily :: Lens.Lens' CreateManagedPrefixList Lude.Text
cmplAddressFamily = Lens.lens (addressFamily :: CreateManagedPrefixList -> Lude.Text) (\s a -> s {addressFamily = a} :: CreateManagedPrefixList)
{-# DEPRECATED cmplAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

instance Lude.AWSRequest CreateManagedPrefixList where
  type Rs CreateManagedPrefixList = CreateManagedPrefixListResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateManagedPrefixListResponse'
            Lude.<$> (x Lude..@? "prefixList") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateManagedPrefixList where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateManagedPrefixList where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateManagedPrefixList where
  toQuery CreateManagedPrefixList' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateManagedPrefixList" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery (Lude.toQueryList "Entry" Lude.<$> entries),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "PrefixListName" Lude.=: prefixListName,
        "MaxEntries" Lude.=: maxEntries,
        "AddressFamily" Lude.=: addressFamily
      ]

-- | /See:/ 'mkCreateManagedPrefixListResponse' smart constructor.
data CreateManagedPrefixListResponse = CreateManagedPrefixListResponse'
  { prefixList ::
      Lude.Maybe
        ManagedPrefixList,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateManagedPrefixListResponse' with the minimum fields required to make a request.
--
-- * 'prefixList' - Information about the prefix list.
-- * 'responseStatus' - The response status code.
mkCreateManagedPrefixListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateManagedPrefixListResponse
mkCreateManagedPrefixListResponse pResponseStatus_ =
  CreateManagedPrefixListResponse'
    { prefixList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the prefix list.
--
-- /Note:/ Consider using 'prefixList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplrsPrefixList :: Lens.Lens' CreateManagedPrefixListResponse (Lude.Maybe ManagedPrefixList)
cmplrsPrefixList = Lens.lens (prefixList :: CreateManagedPrefixListResponse -> Lude.Maybe ManagedPrefixList) (\s a -> s {prefixList = a} :: CreateManagedPrefixListResponse)
{-# DEPRECATED cmplrsPrefixList "Use generic-lens or generic-optics with 'prefixList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmplrsResponseStatus :: Lens.Lens' CreateManagedPrefixListResponse Lude.Int
cmplrsResponseStatus = Lens.lens (responseStatus :: CreateManagedPrefixListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateManagedPrefixListResponse)
{-# DEPRECATED cmplrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
