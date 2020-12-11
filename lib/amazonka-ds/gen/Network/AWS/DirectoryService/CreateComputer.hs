{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateComputer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Active Directory computer object in the specified directory.
module Network.AWS.DirectoryService.CreateComputer
  ( -- * Creating a request
    CreateComputer (..),
    mkCreateComputer,

    -- ** Request lenses
    ccComputerAttributes,
    ccOrganizationalUnitDistinguishedName,
    ccDirectoryId,
    ccComputerName,
    ccPassword,

    -- * Destructuring the response
    CreateComputerResponse (..),
    mkCreateComputerResponse,

    -- ** Response lenses
    ccrsComputer,
    ccrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'CreateComputer' operation.
--
-- /See:/ 'mkCreateComputer' smart constructor.
data CreateComputer = CreateComputer'
  { computerAttributes ::
      Lude.Maybe [Attribute],
    organizationalUnitDistinguishedName :: Lude.Maybe Lude.Text,
    directoryId :: Lude.Text,
    computerName :: Lude.Text,
    password :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateComputer' with the minimum fields required to make a request.
--
-- * 'computerAttributes' - An array of 'Attribute' objects that contain any LDAP attributes to apply to the computer account.
-- * 'computerName' - The name of the computer account.
-- * 'directoryId' - The identifier of the directory in which to create the computer account.
-- * 'organizationalUnitDistinguishedName' - The fully-qualified distinguished name of the organizational unit to place the computer account in.
-- * 'password' - A one-time password that is used to join the computer to the directory. You should generate a random, strong password to use for this parameter.
mkCreateComputer ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'computerName'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  CreateComputer
mkCreateComputer pDirectoryId_ pComputerName_ pPassword_ =
  CreateComputer'
    { computerAttributes = Lude.Nothing,
      organizationalUnitDistinguishedName = Lude.Nothing,
      directoryId = pDirectoryId_,
      computerName = pComputerName_,
      password = pPassword_
    }

-- | An array of 'Attribute' objects that contain any LDAP attributes to apply to the computer account.
--
-- /Note:/ Consider using 'computerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccComputerAttributes :: Lens.Lens' CreateComputer (Lude.Maybe [Attribute])
ccComputerAttributes = Lens.lens (computerAttributes :: CreateComputer -> Lude.Maybe [Attribute]) (\s a -> s {computerAttributes = a} :: CreateComputer)
{-# DEPRECATED ccComputerAttributes "Use generic-lens or generic-optics with 'computerAttributes' instead." #-}

-- | The fully-qualified distinguished name of the organizational unit to place the computer account in.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccOrganizationalUnitDistinguishedName :: Lens.Lens' CreateComputer (Lude.Maybe Lude.Text)
ccOrganizationalUnitDistinguishedName = Lens.lens (organizationalUnitDistinguishedName :: CreateComputer -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnitDistinguishedName = a} :: CreateComputer)
{-# DEPRECATED ccOrganizationalUnitDistinguishedName "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedName' instead." #-}

-- | The identifier of the directory in which to create the computer account.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDirectoryId :: Lens.Lens' CreateComputer Lude.Text
ccDirectoryId = Lens.lens (directoryId :: CreateComputer -> Lude.Text) (\s a -> s {directoryId = a} :: CreateComputer)
{-# DEPRECATED ccDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the computer account.
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccComputerName :: Lens.Lens' CreateComputer Lude.Text
ccComputerName = Lens.lens (computerName :: CreateComputer -> Lude.Text) (\s a -> s {computerName = a} :: CreateComputer)
{-# DEPRECATED ccComputerName "Use generic-lens or generic-optics with 'computerName' instead." #-}

-- | A one-time password that is used to join the computer to the directory. You should generate a random, strong password to use for this parameter.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPassword :: Lens.Lens' CreateComputer (Lude.Sensitive Lude.Text)
ccPassword = Lens.lens (password :: CreateComputer -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: CreateComputer)
{-# DEPRECATED ccPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest CreateComputer where
  type Rs CreateComputer = CreateComputerResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateComputerResponse'
            Lude.<$> (x Lude..?> "Computer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateComputer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.CreateComputer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateComputer where
  toJSON CreateComputer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ComputerAttributes" Lude..=) Lude.<$> computerAttributes,
            ("OrganizationalUnitDistinguishedName" Lude..=)
              Lude.<$> organizationalUnitDistinguishedName,
            Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("ComputerName" Lude..= computerName),
            Lude.Just ("Password" Lude..= password)
          ]
      )

instance Lude.ToPath CreateComputer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateComputer where
  toQuery = Lude.const Lude.mempty

-- | Contains the results for the 'CreateComputer' operation.
--
-- /See:/ 'mkCreateComputerResponse' smart constructor.
data CreateComputerResponse = CreateComputerResponse'
  { computer ::
      Lude.Maybe Computer,
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

-- | Creates a value of 'CreateComputerResponse' with the minimum fields required to make a request.
--
-- * 'computer' - A 'Computer' object that represents the computer account.
-- * 'responseStatus' - The response status code.
mkCreateComputerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateComputerResponse
mkCreateComputerResponse pResponseStatus_ =
  CreateComputerResponse'
    { computer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'Computer' object that represents the computer account.
--
-- /Note:/ Consider using 'computer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsComputer :: Lens.Lens' CreateComputerResponse (Lude.Maybe Computer)
ccrsComputer = Lens.lens (computer :: CreateComputerResponse -> Lude.Maybe Computer) (\s a -> s {computer = a} :: CreateComputerResponse)
{-# DEPRECATED ccrsComputer "Use generic-lens or generic-optics with 'computer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateComputerResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateComputerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateComputerResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
