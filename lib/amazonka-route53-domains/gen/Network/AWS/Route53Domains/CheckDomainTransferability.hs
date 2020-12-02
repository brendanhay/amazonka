{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CheckDomainTransferability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks whether a domain name can be transferred to Amazon Route 53.
module Network.AWS.Route53Domains.CheckDomainTransferability
  ( -- * Creating a Request
    checkDomainTransferability,
    CheckDomainTransferability,

    -- * Request Lenses
    cdtAuthCode,
    cdtDomainName,

    -- * Destructuring the Response
    checkDomainTransferabilityResponse,
    CheckDomainTransferabilityResponse,

    -- * Response Lenses
    cdtrsResponseStatus,
    cdtrsTransferability,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types

-- | The CheckDomainTransferability request contains the following elements.
--
--
--
-- /See:/ 'checkDomainTransferability' smart constructor.
data CheckDomainTransferability = CheckDomainTransferability'
  { _cdtAuthCode ::
      !(Maybe (Sensitive Text)),
    _cdtDomainName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CheckDomainTransferability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdtAuthCode' - If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
--
-- * 'cdtDomainName' - The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . The domain name can contain only the following characters:     * Letters a through z. Domain names are not case sensitive.     * Numbers 0 through 9.     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label.      * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
checkDomainTransferability ::
  -- | 'cdtDomainName'
  Text ->
  CheckDomainTransferability
checkDomainTransferability pDomainName_ =
  CheckDomainTransferability'
    { _cdtAuthCode = Nothing,
      _cdtDomainName = pDomainName_
    }

-- | If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
cdtAuthCode :: Lens' CheckDomainTransferability (Maybe Text)
cdtAuthCode = lens _cdtAuthCode (\s a -> s {_cdtAuthCode = a}) . mapping _Sensitive

-- | The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . The domain name can contain only the following characters:     * Letters a through z. Domain names are not case sensitive.     * Numbers 0 through 9.     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label.      * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
cdtDomainName :: Lens' CheckDomainTransferability Text
cdtDomainName = lens _cdtDomainName (\s a -> s {_cdtDomainName = a})

instance AWSRequest CheckDomainTransferability where
  type
    Rs CheckDomainTransferability =
      CheckDomainTransferabilityResponse
  request = postJSON route53Domains
  response =
    receiveJSON
      ( \s h x ->
          CheckDomainTransferabilityResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "Transferability")
      )

instance Hashable CheckDomainTransferability

instance NFData CheckDomainTransferability

instance ToHeaders CheckDomainTransferability where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Route53Domains_v20140515.CheckDomainTransferability" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CheckDomainTransferability where
  toJSON CheckDomainTransferability' {..} =
    object
      ( catMaybes
          [ ("AuthCode" .=) <$> _cdtAuthCode,
            Just ("DomainName" .= _cdtDomainName)
          ]
      )

instance ToPath CheckDomainTransferability where
  toPath = const "/"

instance ToQuery CheckDomainTransferability where
  toQuery = const mempty

-- | The CheckDomainTransferability response includes the following elements.
--
--
--
-- /See:/ 'checkDomainTransferabilityResponse' smart constructor.
data CheckDomainTransferabilityResponse = CheckDomainTransferabilityResponse'
  { _cdtrsResponseStatus ::
      !Int,
    _cdtrsTransferability ::
      !DomainTransferability
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CheckDomainTransferabilityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdtrsResponseStatus' - -- | The response status code.
--
-- * 'cdtrsTransferability' - A complex type that contains information about whether the specified domain can be transferred to Route 53.
checkDomainTransferabilityResponse ::
  -- | 'cdtrsResponseStatus'
  Int ->
  -- | 'cdtrsTransferability'
  DomainTransferability ->
  CheckDomainTransferabilityResponse
checkDomainTransferabilityResponse
  pResponseStatus_
  pTransferability_ =
    CheckDomainTransferabilityResponse'
      { _cdtrsResponseStatus =
          pResponseStatus_,
        _cdtrsTransferability = pTransferability_
      }

-- | -- | The response status code.
cdtrsResponseStatus :: Lens' CheckDomainTransferabilityResponse Int
cdtrsResponseStatus = lens _cdtrsResponseStatus (\s a -> s {_cdtrsResponseStatus = a})

-- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
cdtrsTransferability :: Lens' CheckDomainTransferabilityResponse DomainTransferability
cdtrsTransferability = lens _cdtrsTransferability (\s a -> s {_cdtrsTransferability = a})

instance NFData CheckDomainTransferabilityResponse
