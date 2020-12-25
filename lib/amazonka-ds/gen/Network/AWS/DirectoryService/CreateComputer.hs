{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ccDirectoryId,
    ccComputerName,
    ccPassword,
    ccComputerAttributes,
    ccOrganizationalUnitDistinguishedName,

    -- * Destructuring the response
    CreateComputerResponse (..),
    mkCreateComputerResponse,

    -- ** Response lenses
    ccrrsComputer,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateComputer' operation.
--
-- /See:/ 'mkCreateComputer' smart constructor.
data CreateComputer = CreateComputer'
  { -- | The identifier of the directory in which to create the computer account.
    directoryId :: Types.DirectoryId,
    -- | The name of the computer account.
    computerName :: Types.ComputerName,
    -- | A one-time password that is used to join the computer to the directory. You should generate a random, strong password to use for this parameter.
    password :: Types.Password,
    -- | An array of 'Attribute' objects that contain any LDAP attributes to apply to the computer account.
    computerAttributes :: Core.Maybe [Types.Attribute],
    -- | The fully-qualified distinguished name of the organizational unit to place the computer account in.
    organizationalUnitDistinguishedName :: Core.Maybe Types.OrganizationalUnitDN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateComputer' value with any optional fields omitted.
mkCreateComputer ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'computerName'
  Types.ComputerName ->
  -- | 'password'
  Types.Password ->
  CreateComputer
mkCreateComputer directoryId computerName password =
  CreateComputer'
    { directoryId,
      computerName,
      password,
      computerAttributes = Core.Nothing,
      organizationalUnitDistinguishedName = Core.Nothing
    }

-- | The identifier of the directory in which to create the computer account.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDirectoryId :: Lens.Lens' CreateComputer Types.DirectoryId
ccDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED ccDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the computer account.
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccComputerName :: Lens.Lens' CreateComputer Types.ComputerName
ccComputerName = Lens.field @"computerName"
{-# DEPRECATED ccComputerName "Use generic-lens or generic-optics with 'computerName' instead." #-}

-- | A one-time password that is used to join the computer to the directory. You should generate a random, strong password to use for this parameter.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPassword :: Lens.Lens' CreateComputer Types.Password
ccPassword = Lens.field @"password"
{-# DEPRECATED ccPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | An array of 'Attribute' objects that contain any LDAP attributes to apply to the computer account.
--
-- /Note:/ Consider using 'computerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccComputerAttributes :: Lens.Lens' CreateComputer (Core.Maybe [Types.Attribute])
ccComputerAttributes = Lens.field @"computerAttributes"
{-# DEPRECATED ccComputerAttributes "Use generic-lens or generic-optics with 'computerAttributes' instead." #-}

-- | The fully-qualified distinguished name of the organizational unit to place the computer account in.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccOrganizationalUnitDistinguishedName :: Lens.Lens' CreateComputer (Core.Maybe Types.OrganizationalUnitDN)
ccOrganizationalUnitDistinguishedName = Lens.field @"organizationalUnitDistinguishedName"
{-# DEPRECATED ccOrganizationalUnitDistinguishedName "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedName' instead." #-}

instance Core.FromJSON CreateComputer where
  toJSON CreateComputer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("ComputerName" Core..= computerName),
            Core.Just ("Password" Core..= password),
            ("ComputerAttributes" Core..=) Core.<$> computerAttributes,
            ("OrganizationalUnitDistinguishedName" Core..=)
              Core.<$> organizationalUnitDistinguishedName
          ]
      )

instance Core.AWSRequest CreateComputer where
  type Rs CreateComputer = CreateComputerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.CreateComputer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComputerResponse'
            Core.<$> (x Core..:? "Computer") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the results for the 'CreateComputer' operation.
--
-- /See:/ 'mkCreateComputerResponse' smart constructor.
data CreateComputerResponse = CreateComputerResponse'
  { -- | A 'Computer' object that represents the computer account.
    computer :: Core.Maybe Types.Computer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateComputerResponse' value with any optional fields omitted.
mkCreateComputerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateComputerResponse
mkCreateComputerResponse responseStatus =
  CreateComputerResponse' {computer = Core.Nothing, responseStatus}

-- | A 'Computer' object that represents the computer account.
--
-- /Note:/ Consider using 'computer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsComputer :: Lens.Lens' CreateComputerResponse (Core.Maybe Types.Computer)
ccrrsComputer = Lens.field @"computer"
{-# DEPRECATED ccrrsComputer "Use generic-lens or generic-optics with 'computer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateComputerResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
