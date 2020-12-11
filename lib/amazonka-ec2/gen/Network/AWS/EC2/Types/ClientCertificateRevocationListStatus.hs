-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
  ( ClientCertificateRevocationListStatus (..),

    -- * Smart constructor
    mkClientCertificateRevocationListStatus,

    -- * Lenses
    ccrlsCode,
    ccrlsMessage,
  )
where

import Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a client certificate revocation list.
--
-- /See:/ 'mkClientCertificateRevocationListStatus' smart constructor.
data ClientCertificateRevocationListStatus = ClientCertificateRevocationListStatus'
  { code ::
      Lude.Maybe
        ClientCertificateRevocationListStatusCode,
    message ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientCertificateRevocationListStatus' with the minimum fields required to make a request.
--
-- * 'code' - The state of the client certificate revocation list.
-- * 'message' - A message about the status of the client certificate revocation list, if applicable.
mkClientCertificateRevocationListStatus ::
  ClientCertificateRevocationListStatus
mkClientCertificateRevocationListStatus =
  ClientCertificateRevocationListStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The state of the client certificate revocation list.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrlsCode :: Lens.Lens' ClientCertificateRevocationListStatus (Lude.Maybe ClientCertificateRevocationListStatusCode)
ccrlsCode = Lens.lens (code :: ClientCertificateRevocationListStatus -> Lude.Maybe ClientCertificateRevocationListStatusCode) (\s a -> s {code = a} :: ClientCertificateRevocationListStatus)
{-# DEPRECATED ccrlsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A message about the status of the client certificate revocation list, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrlsMessage :: Lens.Lens' ClientCertificateRevocationListStatus (Lude.Maybe Lude.Text)
ccrlsMessage = Lens.lens (message :: ClientCertificateRevocationListStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ClientCertificateRevocationListStatus)
{-# DEPRECATED ccrlsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ClientCertificateRevocationListStatus where
  parseXML x =
    ClientCertificateRevocationListStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
