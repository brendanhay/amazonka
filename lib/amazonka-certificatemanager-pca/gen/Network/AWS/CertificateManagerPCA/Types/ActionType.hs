-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ActionType
  ( ActionType
      ( ActionType',
        GetCertificate,
        IssueCertificate,
        ListPermissions
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionType = ActionType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern GetCertificate :: ActionType
pattern GetCertificate = ActionType' "GetCertificate"

pattern IssueCertificate :: ActionType
pattern IssueCertificate = ActionType' "IssueCertificate"

pattern ListPermissions :: ActionType
pattern ListPermissions = ActionType' "ListPermissions"

{-# COMPLETE
  GetCertificate,
  IssueCertificate,
  ListPermissions,
  ActionType'
  #-}
