{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantListEntry where

import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantOperation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a grant.
--
--
--
-- /See:/ 'grantListEntry' smart constructor.
data GrantListEntry = GrantListEntry'
  { _gleKeyId :: !(Maybe Text),
    _gleRetiringPrincipal :: !(Maybe Text),
    _gleIssuingAccount :: !(Maybe Text),
    _gleGrantId :: !(Maybe Text),
    _gleConstraints :: !(Maybe GrantConstraints),
    _gleGranteePrincipal :: !(Maybe Text),
    _gleName :: !(Maybe Text),
    _gleCreationDate :: !(Maybe POSIX),
    _gleOperations :: !(Maybe [GrantOperation])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GrantListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gleKeyId' - The unique identifier for the customer master key (CMK) to which the grant applies.
--
-- * 'gleRetiringPrincipal' - The principal that can retire the grant.
--
-- * 'gleIssuingAccount' - The AWS account under which the grant was issued.
--
-- * 'gleGrantId' - The unique identifier for the grant.
--
-- * 'gleConstraints' - A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
--
-- * 'gleGranteePrincipal' - The identity that gets the permissions in the grant. The @GranteePrincipal@ field in the @ListGrants@ response usually contains the user or role designated as the grantee principal in the grant. However, when the grantee principal in the grant is an AWS service, the @GranteePrincipal@ field contains the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal> , which might represent several different grantee principals.
--
-- * 'gleName' - The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
--
-- * 'gleCreationDate' - The date and time when the grant was created.
--
-- * 'gleOperations' - The list of operations permitted by the grant.
grantListEntry ::
  GrantListEntry
grantListEntry =
  GrantListEntry'
    { _gleKeyId = Nothing,
      _gleRetiringPrincipal = Nothing,
      _gleIssuingAccount = Nothing,
      _gleGrantId = Nothing,
      _gleConstraints = Nothing,
      _gleGranteePrincipal = Nothing,
      _gleName = Nothing,
      _gleCreationDate = Nothing,
      _gleOperations = Nothing
    }

-- | The unique identifier for the customer master key (CMK) to which the grant applies.
gleKeyId :: Lens' GrantListEntry (Maybe Text)
gleKeyId = lens _gleKeyId (\s a -> s {_gleKeyId = a})

-- | The principal that can retire the grant.
gleRetiringPrincipal :: Lens' GrantListEntry (Maybe Text)
gleRetiringPrincipal = lens _gleRetiringPrincipal (\s a -> s {_gleRetiringPrincipal = a})

-- | The AWS account under which the grant was issued.
gleIssuingAccount :: Lens' GrantListEntry (Maybe Text)
gleIssuingAccount = lens _gleIssuingAccount (\s a -> s {_gleIssuingAccount = a})

-- | The unique identifier for the grant.
gleGrantId :: Lens' GrantListEntry (Maybe Text)
gleGrantId = lens _gleGrantId (\s a -> s {_gleGrantId = a})

-- | A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
gleConstraints :: Lens' GrantListEntry (Maybe GrantConstraints)
gleConstraints = lens _gleConstraints (\s a -> s {_gleConstraints = a})

-- | The identity that gets the permissions in the grant. The @GranteePrincipal@ field in the @ListGrants@ response usually contains the user or role designated as the grantee principal in the grant. However, when the grantee principal in the grant is an AWS service, the @GranteePrincipal@ field contains the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal> , which might represent several different grantee principals.
gleGranteePrincipal :: Lens' GrantListEntry (Maybe Text)
gleGranteePrincipal = lens _gleGranteePrincipal (\s a -> s {_gleGranteePrincipal = a})

-- | The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
gleName :: Lens' GrantListEntry (Maybe Text)
gleName = lens _gleName (\s a -> s {_gleName = a})

-- | The date and time when the grant was created.
gleCreationDate :: Lens' GrantListEntry (Maybe UTCTime)
gleCreationDate = lens _gleCreationDate (\s a -> s {_gleCreationDate = a}) . mapping _Time

-- | The list of operations permitted by the grant.
gleOperations :: Lens' GrantListEntry [GrantOperation]
gleOperations = lens _gleOperations (\s a -> s {_gleOperations = a}) . _Default . _Coerce

instance FromJSON GrantListEntry where
  parseJSON =
    withObject
      "GrantListEntry"
      ( \x ->
          GrantListEntry'
            <$> (x .:? "KeyId")
            <*> (x .:? "RetiringPrincipal")
            <*> (x .:? "IssuingAccount")
            <*> (x .:? "GrantId")
            <*> (x .:? "Constraints")
            <*> (x .:? "GranteePrincipal")
            <*> (x .:? "Name")
            <*> (x .:? "CreationDate")
            <*> (x .:? "Operations" .!= mempty)
      )

instance Hashable GrantListEntry

instance NFData GrantListEntry
