{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AccountSharingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AccountSharingInfo
  ( AccountSharingInfo (..)
  -- * Smart constructor
  , mkAccountSharingInfo
  -- * Lenses
  , asiAccountId
  , asiSharedDocumentVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AccountId as Types
import qualified Network.AWS.SSM.Types.SharedDocumentVersion as Types

-- | Information includes the AWS account ID where the current document is shared and the version shared with that account.
--
-- /See:/ 'mkAccountSharingInfo' smart constructor.
data AccountSharingInfo = AccountSharingInfo'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The AWS account ID where the current document is shared.
  , sharedDocumentVersion :: Core.Maybe Types.SharedDocumentVersion
    -- ^ The version of the current document shared with the account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountSharingInfo' value with any optional fields omitted.
mkAccountSharingInfo
    :: AccountSharingInfo
mkAccountSharingInfo
  = AccountSharingInfo'{accountId = Core.Nothing,
                        sharedDocumentVersion = Core.Nothing}

-- | The AWS account ID where the current document is shared.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiAccountId :: Lens.Lens' AccountSharingInfo (Core.Maybe Types.AccountId)
asiAccountId = Lens.field @"accountId"
{-# INLINEABLE asiAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The version of the current document shared with the account.
--
-- /Note:/ Consider using 'sharedDocumentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiSharedDocumentVersion :: Lens.Lens' AccountSharingInfo (Core.Maybe Types.SharedDocumentVersion)
asiSharedDocumentVersion = Lens.field @"sharedDocumentVersion"
{-# INLINEABLE asiSharedDocumentVersion #-}
{-# DEPRECATED sharedDocumentVersion "Use generic-lens or generic-optics with 'sharedDocumentVersion' instead"  #-}

instance Core.FromJSON AccountSharingInfo where
        parseJSON
          = Core.withObject "AccountSharingInfo" Core.$
              \ x ->
                AccountSharingInfo' Core.<$>
                  (x Core..:? "AccountId") Core.<*>
                    x Core..:? "SharedDocumentVersion"
