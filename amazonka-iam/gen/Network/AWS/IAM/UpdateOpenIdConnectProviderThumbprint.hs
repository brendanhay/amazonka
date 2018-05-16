{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the existing list of server certificate thumbprints associated with an OpenID Connect (OIDC) provider resource object with a new list of thumbprints.
--
--
-- The list that you pass with this operation completely replaces the existing list of thumbprints. (The lists are not merged.)
--
-- Typically, you need to update a thumbprint only when the identity provider's certificate changes, which occurs rarely. However, if the provider's certificate /does/ change, any attempt to assume an IAM role that specifies the OIDC provider as a principal fails until the certificate thumbprint is updated.
--
module Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint
    (
    -- * Creating a Request
      updateOpenIdConnectProviderThumbprint
    , UpdateOpenIdConnectProviderThumbprint
    -- * Request Lenses
    , uoicptOpenIdConnectProviderARN
    , uoicptThumbprintList

    -- * Destructuring the Response
    , updateOpenIdConnectProviderThumbprintResponse
    , UpdateOpenIdConnectProviderThumbprintResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateOpenIdConnectProviderThumbprint' smart constructor.
data UpdateOpenIdConnectProviderThumbprint = UpdateOpenIdConnectProviderThumbprint'
  { _uoicptOpenIdConnectProviderARN :: !Text
  , _uoicptThumbprintList           :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateOpenIdConnectProviderThumbprint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoicptOpenIdConnectProviderARN' - The Amazon Resource Name (ARN) of the IAM OIDC provider resource object for which you want to update the thumbprint. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'uoicptThumbprintList' - A list of certificate thumbprints that are associated with the specified IAM OpenID Connect provider. For more information, see 'CreateOpenIDConnectProvider' .
updateOpenIdConnectProviderThumbprint
    :: Text -- ^ 'uoicptOpenIdConnectProviderARN'
    -> UpdateOpenIdConnectProviderThumbprint
updateOpenIdConnectProviderThumbprint pOpenIdConnectProviderARN_ =
  UpdateOpenIdConnectProviderThumbprint'
    { _uoicptOpenIdConnectProviderARN = pOpenIdConnectProviderARN_
    , _uoicptThumbprintList = mempty
    }


-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object for which you want to update the thumbprint. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
uoicptOpenIdConnectProviderARN :: Lens' UpdateOpenIdConnectProviderThumbprint Text
uoicptOpenIdConnectProviderARN = lens _uoicptOpenIdConnectProviderARN (\ s a -> s{_uoicptOpenIdConnectProviderARN = a})

-- | A list of certificate thumbprints that are associated with the specified IAM OpenID Connect provider. For more information, see 'CreateOpenIDConnectProvider' .
uoicptThumbprintList :: Lens' UpdateOpenIdConnectProviderThumbprint [Text]
uoicptThumbprintList = lens _uoicptThumbprintList (\ s a -> s{_uoicptThumbprintList = a}) . _Coerce

instance AWSRequest
           UpdateOpenIdConnectProviderThumbprint
         where
        type Rs UpdateOpenIdConnectProviderThumbprint =
             UpdateOpenIdConnectProviderThumbprintResponse
        request = postQuery iam
        response
          = receiveNull
              UpdateOpenIdConnectProviderThumbprintResponse'

instance Hashable
           UpdateOpenIdConnectProviderThumbprint
         where

instance NFData UpdateOpenIdConnectProviderThumbprint
         where

instance ToHeaders
           UpdateOpenIdConnectProviderThumbprint
         where
        toHeaders = const mempty

instance ToPath UpdateOpenIdConnectProviderThumbprint
         where
        toPath = const "/"

instance ToQuery
           UpdateOpenIdConnectProviderThumbprint
         where
        toQuery UpdateOpenIdConnectProviderThumbprint'{..}
          = mconcat
              ["Action" =:
                 ("UpdateOpenIDConnectProviderThumbprint" ::
                    ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _uoicptOpenIdConnectProviderARN,
               "ThumbprintList" =:
                 toQueryList "member" _uoicptThumbprintList]

-- | /See:/ 'updateOpenIdConnectProviderThumbprintResponse' smart constructor.
data UpdateOpenIdConnectProviderThumbprintResponse =
  UpdateOpenIdConnectProviderThumbprintResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateOpenIdConnectProviderThumbprintResponse' with the minimum fields required to make a request.
--
updateOpenIdConnectProviderThumbprintResponse
    :: UpdateOpenIdConnectProviderThumbprintResponse
updateOpenIdConnectProviderThumbprintResponse =
  UpdateOpenIdConnectProviderThumbprintResponse'


instance NFData
           UpdateOpenIdConnectProviderThumbprintResponse
         where
