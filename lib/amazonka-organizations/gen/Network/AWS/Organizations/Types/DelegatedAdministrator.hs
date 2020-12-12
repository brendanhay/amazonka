{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.DelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.DelegatedAdministrator
  ( DelegatedAdministrator (..),

    -- * Smart constructor
    mkDelegatedAdministrator,

    -- * Lenses
    daStatus,
    daJoinedMethod,
    daEmail,
    daARN,
    daJoinedTimestamp,
    daDelegationEnabledDate,
    daName,
    daId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.AccountJoinedMethod
import Network.AWS.Organizations.Types.AccountStatus
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the delegated administrator.
--
-- /See:/ 'mkDelegatedAdministrator' smart constructor.
data DelegatedAdministrator = DelegatedAdministrator'
  { status ::
      Lude.Maybe AccountStatus,
    joinedMethod ::
      Lude.Maybe AccountJoinedMethod,
    email ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    arn :: Lude.Maybe Lude.Text,
    joinedTimestamp :: Lude.Maybe Lude.Timestamp,
    delegationEnabledDate ::
      Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe (Lude.Sensitive Lude.Text),
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DelegatedAdministrator' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the delegated administrator's account.
-- * 'delegationEnabledDate' - The date when the account was made a delegated administrator.
-- * 'email' - The email address that is associated with the delegated administrator's AWS account.
-- * 'id' - The unique identifier (ID) of the delegated administrator's account.
-- * 'joinedMethod' - The method by which the delegated administrator's account joined the organization.
-- * 'joinedTimestamp' - The date when the delegated administrator's account became a part of the organization.
-- * 'name' - The friendly name of the delegated administrator's account.
-- * 'status' - The status of the delegated administrator's account in the organization.
mkDelegatedAdministrator ::
  DelegatedAdministrator
mkDelegatedAdministrator =
  DelegatedAdministrator'
    { status = Lude.Nothing,
      joinedMethod = Lude.Nothing,
      email = Lude.Nothing,
      arn = Lude.Nothing,
      joinedTimestamp = Lude.Nothing,
      delegationEnabledDate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The status of the delegated administrator's account in the organization.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStatus :: Lens.Lens' DelegatedAdministrator (Lude.Maybe AccountStatus)
daStatus = Lens.lens (status :: DelegatedAdministrator -> Lude.Maybe AccountStatus) (\s a -> s {status = a} :: DelegatedAdministrator)
{-# DEPRECATED daStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The method by which the delegated administrator's account joined the organization.
--
-- /Note:/ Consider using 'joinedMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daJoinedMethod :: Lens.Lens' DelegatedAdministrator (Lude.Maybe AccountJoinedMethod)
daJoinedMethod = Lens.lens (joinedMethod :: DelegatedAdministrator -> Lude.Maybe AccountJoinedMethod) (\s a -> s {joinedMethod = a} :: DelegatedAdministrator)
{-# DEPRECATED daJoinedMethod "Use generic-lens or generic-optics with 'joinedMethod' instead." #-}

-- | The email address that is associated with the delegated administrator's AWS account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daEmail :: Lens.Lens' DelegatedAdministrator (Lude.Maybe (Lude.Sensitive Lude.Text))
daEmail = Lens.lens (email :: DelegatedAdministrator -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {email = a} :: DelegatedAdministrator)
{-# DEPRECATED daEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The Amazon Resource Name (ARN) of the delegated administrator's account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daARN :: Lens.Lens' DelegatedAdministrator (Lude.Maybe Lude.Text)
daARN = Lens.lens (arn :: DelegatedAdministrator -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DelegatedAdministrator)
{-# DEPRECATED daARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the delegated administrator's account became a part of the organization.
--
-- /Note:/ Consider using 'joinedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daJoinedTimestamp :: Lens.Lens' DelegatedAdministrator (Lude.Maybe Lude.Timestamp)
daJoinedTimestamp = Lens.lens (joinedTimestamp :: DelegatedAdministrator -> Lude.Maybe Lude.Timestamp) (\s a -> s {joinedTimestamp = a} :: DelegatedAdministrator)
{-# DEPRECATED daJoinedTimestamp "Use generic-lens or generic-optics with 'joinedTimestamp' instead." #-}

-- | The date when the account was made a delegated administrator.
--
-- /Note:/ Consider using 'delegationEnabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDelegationEnabledDate :: Lens.Lens' DelegatedAdministrator (Lude.Maybe Lude.Timestamp)
daDelegationEnabledDate = Lens.lens (delegationEnabledDate :: DelegatedAdministrator -> Lude.Maybe Lude.Timestamp) (\s a -> s {delegationEnabledDate = a} :: DelegatedAdministrator)
{-# DEPRECATED daDelegationEnabledDate "Use generic-lens or generic-optics with 'delegationEnabledDate' instead." #-}

-- | The friendly name of the delegated administrator's account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DelegatedAdministrator (Lude.Maybe (Lude.Sensitive Lude.Text))
daName = Lens.lens (name :: DelegatedAdministrator -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {name = a} :: DelegatedAdministrator)
{-# DEPRECATED daName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier (ID) of the delegated administrator's account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daId :: Lens.Lens' DelegatedAdministrator (Lude.Maybe Lude.Text)
daId = Lens.lens (id :: DelegatedAdministrator -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DelegatedAdministrator)
{-# DEPRECATED daId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON DelegatedAdministrator where
  parseJSON =
    Lude.withObject
      "DelegatedAdministrator"
      ( \x ->
          DelegatedAdministrator'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "JoinedMethod")
            Lude.<*> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "JoinedTimestamp")
            Lude.<*> (x Lude..:? "DelegationEnabledDate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
